document.getElementsByClassName('chip-close').onclick = function() {
  this.parentNode.parentNode.parentNode
    .removeChild(this.parentNode.parentNode);
  return false;
};
