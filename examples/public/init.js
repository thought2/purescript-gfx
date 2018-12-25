function main() {
    var canvas = document.getElementById("glcanvas");

    var resize = function() {
        canvas.width = window.innerWidth;
        canvas.height = window.innerHeight;
    }

    resize();
    window.onresize = resize;
}

main();
