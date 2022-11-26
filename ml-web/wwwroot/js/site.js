$(document).ready(function () {
    const editor = CodeMirror.fromTextArea(document.getElementById("term"), {
        lineNumbers: true,
        mode: "mllike",
    });
});