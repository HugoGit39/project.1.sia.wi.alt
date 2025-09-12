/* 3. App info*/

/* This function sets up the citation copy button. */

document.addEventListener("DOMContentLoaded", function () {
  const btn = document.getElementById("copy_citation_btn");
  if (btn) {
    btn.onclick = function () {
      const citation = `Schoenmakers M, Saygin M, Sikora M, Vaessen T, Noordzij M, de Geus E. Stress in action wearables database: A database of noninvasive wearable monitors with systematic technical, reliability, validity, and usability information. Behav Res Methods. 2025 May 13;57(6):171. doi: 10.3758/s13428-025-02685-4. PMID: 40360861; PMCID: PMC12075381.`;
      navigator.clipboard.writeText(citation).then(() => {
        alert("Citations copied to clipboard!");
      });
    };
  }
});

// Data submited
Shiny.addCustomMessageHandler("dataSubmitted", function(message) {
  alert(message);
});

// Email submited
Shiny.addCustomMessageHandler("emailSubmitted", function(message) {
  alert(message);
});



