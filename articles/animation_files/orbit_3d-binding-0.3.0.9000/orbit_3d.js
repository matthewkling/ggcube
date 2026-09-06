HTMLWidgets.widget({

  name: "orbit_3d",
  type: "output",

  factory: function(el, width, height) {

    // ---- Per-instance state -------------------------------------------------
    var srcs = [];
    var nx = 1, ny = 1, n = 0;
    var loopX = false, loopY = false;
    var ix = 0, iy = 0;
    var curIndex = -1;
    var ready = false;

    var dragging = false;
    var startX = 0, startY = 0;
    var startIx = 0, startIy = 0;

    // ---- DOM (built once, owned by this widget instance) --------------------
    // `el` is the widget's container div. We style it as the interactive
    // surface and add an <img> for the current frame plus a loading overlay.
    el.classList.add("ggcube-orbit");
    el.setAttribute("tabindex", "0");
    el.setAttribute("aria-label",
      "3D plot. Drag or use the arrow keys to rotate.");

    var imgEl = document.createElement("img");
    imgEl.className = "ggcube-orbit-frame";
    imgEl.setAttribute("alt", "3D plot (drag or arrow keys to rotate)");
    el.appendChild(imgEl);

    var loading = document.createElement("div");
    loading.className = "ggcube-orbit-loading";
    loading.textContent = "Loading frames...";
    el.appendChild(loading);

    // ---- Index arithmetic ---------------------------------------------------
    function wrapAxis(i, len, loop) {
      if (len <= 1) return 0;
      if (loop) return ((i % len) + len) % len;
      if (i < 0) return 0;
      if (i > len - 1) return len - 1;
      return i;
    }

    function render() {
      ix = wrapAxis(ix, nx, loopX);
      iy = wrapAxis(iy, ny, loopY);
      var idx = iy * nx + ix;
      if (idx === curIndex) return;
      curIndex = idx;
      imgEl.src = srcs[idx];
    }

    function setPos(nix, niy) {
      ix = Math.round(nix);
      iy = Math.round(niy);
      render();
    }

    // ---- Drag ---------------------------------------------------------------
    function ixFromDx(dx) {
      var w = el.clientWidth || 1;
      return startIx + dx * ((nx - 1) / w);
    }
    function iyFromDy(dy) {
      var h = el.clientHeight || 1;
      return startIy + dy * ((ny - 1) / h);
    }

    function endDrag() {
      if (!dragging) return;
      dragging = false;
      el.classList.remove("dragging");
    }

    function onDown(e) {
      if (!ready) return;
      dragging = true;
      el.classList.add("dragging");
      startX = e.clientX; startY = e.clientY;
      startIx = ix; startIy = iy;
      if (el.setPointerCapture && e.pointerId !== undefined) {
        el.setPointerCapture(e.pointerId);
      }
      // preventDefault suppresses the implicit focus a pointerdown gives a
      // tabindex element, so focus explicitly and flag it as pointer focus so
      // the focus ring stays hidden until the keyboard drives.
      el.classList.add("focus-from-pointer");
      el.focus();
      e.preventDefault();
    }

    function onMove(e) {
      if (!dragging) return;
      setPos(ixFromDx(e.clientX - startX), iyFromDy(e.clientY - startY));
      e.preventDefault();
    }

    function onUp() { endDrag(); }

    if (window.PointerEvent) {
      el.addEventListener("pointerdown", onDown);
      // Track and release at the window level so movement continues over other
      // content and release is never missed outside the element.
      window.addEventListener("pointermove", onMove);
      window.addEventListener("pointerup", onUp);
      window.addEventListener("pointercancel", onUp);
    } else {
      el.addEventListener("mousedown", onDown);
      window.addEventListener("mousemove", onMove);
      window.addEventListener("mouseup", onUp);
    }

    // End the drag if focus leaves the page mid-drag.
    window.addEventListener("blur", endDrag);

    // Reset the pointer-focus flag on blur so a later Tab focus shows the ring.
    el.addEventListener("blur", function() {
      el.classList.remove("focus-from-pointer");
    });

    // ---- Keyboard -----------------------------------------------------------
    // Scoped to this element: unlike the single-page player (which listened on
    // document), a widget may share a page with others, so each instance only
    // responds when it itself has focus. This also avoids double-firing.
    el.addEventListener("keydown", function(e) {
      if (!ready) return;
      var handled = false;
      if (e.key === "ArrowLeft" || e.keyCode === 37) {
        setPos(ix - 1, iy); handled = true;
      } else if (e.key === "ArrowRight" || e.keyCode === 39) {
        setPos(ix + 1, iy); handled = true;
      } else if (ny > 1 && (e.key === "ArrowUp" || e.keyCode === 38)) {
        setPos(ix, iy - 1); handled = true;
      } else if (ny > 1 && (e.key === "ArrowDown" || e.keyCode === 40)) {
        setPos(ix, iy + 1); handled = true;
      }
      if (handled) {
        el.classList.remove("focus-from-pointer");
        e.preventDefault();
      }
    });

    // ---- htmlwidgets interface ---------------------------------------------
    return {
      renderValue: function(x) {
        // x carries the flat frame list plus the topology the R side computed:
        //   frames    : array of data-URI (or URL) strings, row-major
        //   dims       : [nx] or [nx, ny]
        //   loop_flags : [loopX] or [loopX, loopY]
        //   start      : [ix] or [ix, iy]  (0-based initial cell)
        srcs = x.frames || [];
        n = srcs.length;

        var dims = x.dims || [n];
        nx = dims[0];
        ny = dims.length >= 2 ? dims[1] : 1;

        var lf = x.loop_flags || [];
        loopX = !!lf[0];
        loopY = lf.length >= 2 ? !!lf[1] : false;

        var st = x.start || [];
        ix = st.length >= 1 ? st[0] : 0;
        iy = st.length >= 2 ? st[1] : 0;

        curIndex = -1;
        ready = false;
        loading.style.display = "";

        if (n === 0) return;

        // Preload all frames, then enable interaction and show the start cell.
        var loaded = 0;
        for (var k = 0; k < n; k++) {
          (function(k) {
            var im = new Image();
            im.onload = im.onerror = function() {
              loaded++;
              if (loaded === n) {
                ready = true;
                loading.style.display = "none";
                render();
              }
            };
            im.src = srcs[k];
          })(k);
        }
      },

      resize: function(newWidth, newHeight) {
        // Frames are fixed-size rasters; CSS (max-width:100%) handles fitting.
        // Nothing to recompute, but the method must exist.
      }
    };
  }
});
