$(() => {

  /* Default installation */

  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}

  $('#heat-nav').on('click', function(e) {
  //ga('send', 'event', 'button', 'update plot');
  const txt = e.target.innerText;
  if(txt === "Explore inequality" || txt === "Compare inequality" || txt ==="About"){
        return;
  } else {
        let par = e.target.parentElement;

        if(par === undefined){
          par = ''
        } else {
          par = par.parentElement.id.replace('heat-', '')
        }
        let selectedTxt = e.target.innerText;
        selectedTxt = selectedTxt.replace(' data', '');
        selectedTxt = selectedTxt.replace(' measures', '');

        const page_name = par + '-' + selectedTxt;
        gtag('event', 'main_navigation', {'page_name': page_name})
        gtag('event',  page_name)
  }
  });



  gtag('js', new Date());
  gtag('config', 'G-L76PYL961C');
  //gtag('config', 'G-HY4MYZ44B9'); // dev, http://127.0.0.1:8080/




});
