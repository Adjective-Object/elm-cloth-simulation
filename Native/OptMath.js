// rsqrt provided by http://jsperf.com/fast-inverse-square-root
console.log("OptMath declared");
Elm.Native.OptMath = {}
Elm.Native.OptMath.make = function(elm){
    console.log("OptMath make");
    elm.Native = elm.Native || {};
    elm.Native.OptMath = elm.Native.OptMath || {};
    
    if (elm.Native.OptMath.values) return elm.Native.OptMath.values;

    var buf = new ArrayBuffer(Float32Array.BYTES_PER_ELEMENT);

    var fv = new Float32Array(buf);
    var lv = new Uint32Array(buf);
    var threehalfs = 1.5;


    function Q_rsqrt(number) {
        var x2 = number * 0.5;
        fv[0] = number;
        lv[0] = 0x5f3759df - ( lv[0] >> 1 );
        var y = fv[0];
        y = y * ( threehalfs - ( x2 * y * y ) );

        return y;
    }

    function Q_sqrt(number) {
      return 1/Q_rsqrt(number);
    }

    return Elm.Native.OptMath.values = {
      Q_rsqrt :  Q_rsqrt,
      Q_sqrt  :  Q_sqrt,
      rsqrt   :  Q_rsqrt,
      sqrt    :  Q_sqrt
    };
}
