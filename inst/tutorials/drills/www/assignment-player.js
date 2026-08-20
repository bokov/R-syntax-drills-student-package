(function() {
  var handlersRegistered = false;
  var currentLabels = [];

  function waitingElement() {
    return document.getElementById('assignment-waiting');
  }

  function assignmentTopic() {
    var waiting = waitingElement();
    return waiting ? waiting.closest('.section.level2') : document;
  }

  function exerciseElements() {
    return Array.prototype.slice.call(
      assignmentTopic().querySelectorAll('.tutorial-exercise[data-label]')
    );
  }

  function questionSection(exercise) {
    if (!exercise) return null;
    var section = exercise.closest('.section.level4');
    if (section) section.classList.add('assignment-question');
    return section;
  }

  function questionSectionForLabel(label) {
    var exercise = exerciseElements().find(function(element) {
      return element.getAttribute('data-label') === label;
    });
    return questionSection(exercise);
  }

  function allQuestionBlocks() {
    var seen = [];
    exerciseElements().forEach(function(exercise) {
      var section = questionSection(exercise);
      if (section && seen.indexOf(section) < 0) seen.push(section);
    });
    return seen;
  }

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

  function markNewQuestion(block) {
    if (!block) return;
    block.classList.add('assignment-question-new');
    window.setTimeout(function() {
      block.classList.remove('assignment-question-new');
    }, 2600);
  }

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

  function clearAssignments() {
    currentLabels = [];
    clearTransitionArtifacts();
    hideAll();
  }

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

  hideAll();
  registerWhenReady();
})();
