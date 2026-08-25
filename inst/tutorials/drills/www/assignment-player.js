(function() {
  // Browser-side assignment state. Shiny handlers are registered once, and the
  // current labels are retained so a one-for-one replacement can be animated.
  var handlersRegistered = false;
  var currentLabels = [];

  /**
   * Finds the status element that occupies the assignment area before drills
   * are available and reports assignment/player errors.
   *
   * Called by assignmentTopic(), hideAll(), and showAssignments().
   * @returns {HTMLElement|null} The assignment waiting/status element.
   */
  function waitingElement() {
    return document.getElementById('assignment-waiting');
  }

  /**
   * Locates the learnr section that contains the assignment exercises so DOM
   * searches stay scoped to the drill player rather than the whole tutorial.
   *
   * Called only by exerciseElements(); depends on waitingElement().
   * @returns {HTMLElement|Document} The assignment section, or document as a
   * fallback when the waiting element is unavailable.
   */
  function assignmentTopic() {
    var waiting = waitingElement();
    return waiting ? waiting.closest('.section.level2') : document;
  }

  /**
   * Returns all learnr exercise elements with question labels in the assignment
   * section.
   *
   * Called by questionSectionForLabel() and allQuestionBlocks(); depends on
   * assignmentTopic().
   * @returns {HTMLElement[]} Labeled tutorial exercise elements.
   */
  function exerciseElements() {
    return Array.prototype.slice.call(
      assignmentTopic().querySelectorAll('.tutorial-exercise[data-label]')
    );
  }

  /**
   * Resolves an exercise element to its surrounding question block and tags the
   * block with the CSS class used by the assignment player.
   *
   * Called by questionSectionForLabel() and allQuestionBlocks().
   * @param {HTMLElement|null} exercise A learnr exercise element.
   * @returns {HTMLElement|null} The containing level-4 section, when present.
   */
  function questionSection(exercise) {
    if (!exercise) return null;
    var section = exercise.closest('.section.level4');
    if (section) section.classList.add('assignment-question');
    return section;
  }

  /**
   * Finds the question block for one manifest/assignment item label.
   *
   * Called by showAssignments() for retired and active labels; depends on
   * exerciseElements() and questionSection().
   * @param {string} label Item label to locate.
   * @returns {HTMLElement|null} The corresponding question block.
   */
  function questionSectionForLabel(label) {
    var exercise = exerciseElements().find(function(element) {
      return element.getAttribute('data-label') === label;
    });
    return questionSection(exercise);
  }

  /**
   * Collects each distinct question block represented by the labeled exercises.
   *
   * Called by clearTransitionArtifacts(), hideAll(), and showAssignments();
   * depends on exerciseElements() and questionSection().
   * @returns {HTMLElement[]} Distinct question block elements.
   */
  function allQuestionBlocks() {
    var seen = [];
    exerciseElements().forEach(function(exercise) {
      var section = questionSection(exercise);
      if (section && seen.indexOf(section) < 0) seen.push(section);
    });
    return seen;
  }

  /**
   * Removes notices and temporary highlight classes left by a previous
   * assignment transition before the next state is rendered.
   *
   * Called by showAssignments() and clearAssignments(); depends on
   * allQuestionBlocks().
   * @returns {void}
   */
  function clearTransitionArtifacts() {
    Array.prototype.slice.call(
      document.querySelectorAll('.assignment-retired-notice')
    ).forEach(function(element) {
      element.remove();
    });
    allQuestionBlocks().forEach(function(block) {
      block.classList.remove('assignment-question-new');
    });
  }

  /**
   * Hides every question and restores the initial waiting message shown before
   * a student identity has loaded an assignment queue.
   *
   * Called by clearAssignments() and once during script startup; depends on
   * allQuestionBlocks() and waitingElement().
   * @returns {void}
   */
  function hideAll() {
    allQuestionBlocks().forEach(function(block) {
      block.style.display = 'none';
    });

    var waiting = waitingElement();
    if (waiting) {
      waiting.style.display = 'block';
      waiting.className = 'alert alert-info';
      waiting.textContent =
        'Your questions will appear here after you load a valid student ID.';
    }
  }

  /**
   * Inserts the short-lived completion notice used when one answered question
   * is retired and replaced by another.
   *
   * Called only by showAssignments().
   * @param {HTMLElement|null} block The retiring question block.
   * @returns {HTMLElement|null} The inserted notice, or null when insertion is
   * not possible.
   */
  function retiredNoticeAt(block) {
    if (!block || !block.parentNode) return null;

    var notice = document.createElement('div');
    notice.className = 'assignment-retired-notice';
    notice.setAttribute('role', 'status');
    notice.textContent = '\u2713 Done \u2014 new drill added below';
    block.parentNode.insertBefore(notice, block);

    window.setTimeout(function() {
      notice.classList.add('assignment-retired-notice-fade');
    }, 850);
    window.setTimeout(function() {
      if (notice.parentNode) notice.parentNode.removeChild(notice);
    }, 1350);

    return notice;
  }

  /**
   * Temporarily highlights a newly assigned question after a one-for-one queue
   * replacement.
   *
   * Called only by showAssignments().
   * @param {HTMLElement|null} block Newly assigned question block.
   * @returns {void}
   */
  function markNewQuestion(block) {
    if (!block) return;
    block.classList.add('assignment-question-new');
    window.setTimeout(function() {
      block.classList.remove('assignment-question-new');
    }, 2600);
  }

  /**
   * Reconciles the visible tutorial questions with the item-label list sent by
   * Shiny, preserving server order and animating a single completed/replacement
   * transition when possible.
   *
   * Registered as the `assignment:set` Shiny message handler by
   * registerShinyHandlers(). Depends on clearTransitionArtifacts(),
   * retiredNoticeAt(), allQuestionBlocks(), questionSectionForLabel(),
   * markNewQuestion(), and waitingElement().
   * @param {{item_labels?: string[]}|null} message Assignment message from Shiny.
   * @returns {void}
   */
  function showAssignments(message) {
    var labels = (message && message.item_labels) || [];
    var previous = currentLabels.slice();
    var removed = previous.filter(function(label) {
      return labels.indexOf(label) < 0;
    });
    var added = labels.filter(function(label) {
      return previous.indexOf(label) < 0;
    });
    var animateReplacement =
      previous.length > 0 && removed.length === 1 && added.length === 1;

    clearTransitionArtifacts();

    if (animateReplacement) {
      retiredNoticeAt(questionSectionForLabel(removed[0]));
    }

    allQuestionBlocks().forEach(function(block) {
      block.style.display = 'none';
    });

    var shown = 0;
    var destination = null;
    labels.forEach(function(label, index) {
      var block = questionSectionForLabel(label);
      if (!block) {
        console.error('Assigned question is missing from the player:', label);
        return;
      }

      if (!destination) destination = block.parentNode;
      if (destination && block.parentNode === destination) {
        destination.appendChild(block);
      }

      block.style.display = 'block';
      block.dataset.assignmentOrder = String(index);
      if (animateReplacement && label === added[0]) markNewQuestion(block);
      shown += 1;
    });

    currentLabels = labels.slice();

    var waiting = waitingElement();
    if (waiting) {
      if (shown === labels.length && shown > 0) {
        waiting.style.display = 'none';
      } else if (labels.length > 0) {
        waiting.style.display = 'block';
        waiting.className = 'alert alert-danger';
        waiting.textContent =
          'Your assignment was created, but one or more assigned questions ' +
          'could not be found in this installed drillr package. Update drillr and reload.';
      } else {
        waiting.style.display = 'block';
      }
    }
  }

  /**
   * Clears the active assignment labels, transition artifacts, and visible
   * question blocks when the R session tells the browser to forget assignments.
   *
   * Called by the `assignment:clear` Shiny message handler registered in
   * registerShinyHandlers(); depends on clearTransitionArtifacts() and hideAll().
   * @returns {void}
   */
  function clearAssignments() {
    currentLabels = [];
    clearTransitionArtifacts();
    hideAll();
  }

  /**
   * Registers the custom Shiny messages that set or clear the browser-side
   * assignment queue, once Shiny's client API is available.
   *
   * Called by registerWhenReady() and its retry timer; depends on
   * showAssignments() and clearAssignments().
   * @returns {boolean} True once handlers are registered, false when Shiny is
   * not ready yet.
   */
  function registerShinyHandlers() {
    if (handlersRegistered) return true;
    if (!window.Shiny || !window.Shiny.addCustomMessageHandler) return false;

    window.Shiny.addCustomMessageHandler('assignment:set', showAssignments);
    window.Shiny.addCustomMessageHandler('assignment:clear', function(message) {
      clearAssignments();
    });
    handlersRegistered = true;
    return true;
  }

  /**
   * Registers Shiny handlers immediately when possible or polls briefly until
   * the Shiny client has initialized.
   *
   * Called once during script startup; depends on registerShinyHandlers().
   * @returns {void}
   */
  function registerWhenReady() {
    if (registerShinyHandlers()) return;

    var attempts = 0;
    var timer = window.setInterval(function() {
      attempts += 1;
      if (registerShinyHandlers() || attempts >= 200) {
        window.clearInterval(timer);
      }
    }, 50);
  }

  // Initialize the player with every drill hidden, then attach Shiny message
  // handlers as soon as the learnr page has made the Shiny client available.
  hideAll();
  registerWhenReady();
})();
