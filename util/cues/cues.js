var g_epoch = 0;
var g_cues = [{time:0,cue:"..."}]

/* fractional minutes are mm.ss, so that 15.35 is 15 minutes and 35 seconds */
function fmin_to_sec(n) {
    var m = Math.floor(n);
    var s = Math.round((n - m) * 100);
    return (m * 60) + s;
}

function sec_to_fmin(n) {
    var m = Math.floor(n / 60);
    var s = n - (m * 60);
    return m + (s / 100);
}

function fmin_sub(i,j) {
    return sec_to_fmin(fmin_to_sec(j) - fmin_to_sec(i));
}

function cues_add_dur (c) {
    var n = c.length;
    for (var i = 0, j = 1; j < n; i++, j++) {
        c[i].dur = fmin_sub(c[i].time,c[j].time);
    }
    c[n - 1].dur = 0;
}

function cues_add_time (c) {
    var t = 0;
    for (var i = 0; i < c.length; i++) {
        c[i].time = sec_to_fmin(t);
        t += fmin_to_sec(c[i].dur);
    }
}

function cues_normalise (c) {
    if(c[0].dur == undefined) {
        cues_add_dur(c);
    } else {
        cues_add_time(c);
    }
}

function ctime_sec() {
    return Math.round(Date.now() / 1000);
}

function sec_to_timestr(n) {
    var d = new Date(n * 1000);
    return d.toUTCString().substr(17,8);
}

function get_cue_ix(c,t) {
    for (var i = 0; i < c.length; i += 1) {
        if(t < fmin_to_sec(c[i].time)) return i - 1;
    }
    return c.length - 1;
}

function get_cue(c,t) {
    return c[get_cue_ix(c,t)].cue;
}

function fmin_to_timestr (t) {
    return sec_to_timestr(fmin_to_sec(t));
}

function set_cue(cues,t_id,c_id,q_id) {
    var tm = ctime_sec() - g_epoch;
    var ix = get_cue_ix(cues,tm);
    var c = cues[ix];
    var q = ix == cues.length - 1 ? c : cues[ix + 1];
    var q_cues = [fmin_to_timestr(c.dur),q.cue,fmin_to_timestr(q.time)];
    document.getElementById(t_id).textContent = sec_to_timestr(tm);
    document.getElementById(c_id).textContent = c.cue;
    document.getElementById(q_id).textContent = q_cues.join(' / ');
}

function reset() {
    cues_normalise(g_cues);
    g_epoch = ctime_sec();
    document.getElementById("data").textContent = JSON.stringify(g_cues);
    set_cue(g_cues,"clock","cue","queue");
}

function load_usr(ev) {
    var f = ev.target.files[0];
    var r = new FileReader();
    r.onload = (function(theFile) {
        return function(e) {
            g_cues = JSON.parse(e.target.result);
            reset();
        };
    })(f);
    r.readAsText(f);
}

document.getElementById('usr').addEventListener('change', load_usr, false);
reset();
setInterval(function() {set_cue(g_cues,"clock","cue","queue");}, 1000);
