// rsqrt provided by http://jsperf.com/fast-inverse-square-root
var _adjective_object$elm_game$Native_OptMath = function() {
    let buf = new ArrayBuffer(Float32Array.BYTES_PER_ELEMENT);

    let fv = new Float32Array(buf);
    let lv = new Uint32Array(buf);
    let threehalfs = 1.5;

    function Q_rsqrt(number) {
        var x2 = number * 0.5;
        fv[0] = number;
        lv[0] = 0x5f3759df - ( lv[0] >> 1 );
        var y = fv[0];
        y = y * ( threehalfs - ( x2 * y * y ) );

        return y;
    };

    function Q_sqrt(number) {
      return 1/Q_rsqrt(number);
    };

    return {
      Q_rsqrt :  Q_rsqrt,
      Q_sqrt  :  Q_sqrt,
      rsqrt   :  Q_rsqrt,
      sqrt    :  Q_sqrt
    };
}();
