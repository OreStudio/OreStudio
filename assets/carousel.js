// ORE Studio — dependency-free screenshot carousel for index.org.
// Auto-advances every 6s, pauses on hover/focus, wraps at both ends.
(function () {
    "use strict";

    var ADVANCE_MS = 6000;

    function initCarousel(root) {
        var slides = root.querySelectorAll(".carousel-slide");
        var dots = root.querySelectorAll(".carousel-dot");
        var prevBtn = root.querySelector(".carousel-nav.prev");
        var nextBtn = root.querySelector(".carousel-nav.next");
        var viewport = root.querySelector(".carousel-viewport");
        var current = 0;
        var timer = null;

        function show(index) {
            current = (index + slides.length) % slides.length;
            slides.forEach(function (slide, i) {
                slide.classList.toggle("is-active", i === current);
            });
            dots.forEach(function (dot, i) {
                dot.classList.toggle("is-active", i === current);
            });
        }

        function next() { show(current + 1); }
        function prev() { show(current - 1); }

        function start() {
            stop();
            timer = window.setInterval(next, ADVANCE_MS);
        }

        function stop() {
            if (timer) {
                window.clearInterval(timer);
                timer = null;
            }
        }

        if (prevBtn) prevBtn.addEventListener("click", function () { prev(); start(); });
        if (nextBtn) nextBtn.addEventListener("click", function () { next(); start(); });
        dots.forEach(function (dot, i) {
            dot.addEventListener("click", function () { show(i); start(); });
        });

        if (viewport) {
            viewport.addEventListener("mouseenter", stop);
            viewport.addEventListener("mouseleave", start);
            viewport.addEventListener("focusin", stop);
            viewport.addEventListener("focusout", start);
        }

        show(0);
        start();
    }

    document.addEventListener("DOMContentLoaded", function () {
        document.querySelectorAll(".carousel").forEach(initCarousel);
    });
})();
