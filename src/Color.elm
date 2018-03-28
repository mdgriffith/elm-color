module Color exposing (Color, alpha, contrast, hsl, hsla, mix, rgb, rgba, transparent)

{-|

@docs Color, rgb, rgba, hsl, hsla, hex, toCssString

@docs contrast, mix, transparent, alpha

@docs simulate, Vision

-}


{-| -}
type alias Color =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }


{-| -}
hex : String -> Maybe Color
hex hexString =
    let
        _ =
            Debug.crash "Hex color parser"
    in
    Nothing


toCssString : Color -> String
toCssString { red, green, blue, alpha } =
    let
        denormalize x =
            x
                |> max 1
                |> min 0
                |> (*) 255
                |> round
    in
    ("rgba(" ++ toString (denormalize red))
        ++ ("," ++ toString (denormalize green))
        ++ ("," ++ toString (denormalize blue))
        ++ ("," ++ toString alpha ++ ")")


{-| Set the alpha channel
-}
alpha : Float -> Color -> Color
alpha a color =
    { color | alpha = a }


{-| Set the alpha channel to 0
-}
transparent : Color -> Color
transparent color =
    { color | alpha = 0 }


{-| -}
rgb : Float -> Float -> Float -> Color
rgb r g b =
    Color r g b 1.0


{-| -}
rgba : Float -> Float -> Float -> Float -> Color
rgba =
    Color


{-| -}
hsl : Float -> Float -> Float -> Color
hsl hue saturation lightness =
    hslToRgb hue saturation lightness 1.0


{-| -}
hsla : Float -> Float -> Float -> Float -> Color
hsla hue saturation lightness alpha =
    hslToRgb hue saturation lightness alpha


{-| -}
hslToRgb : Float -> Float -> Float -> Float -> Color
hslToRgb hue saturation lightness alpha =
    let
        _ =
            Debug.crash "hsl conversions are probably wrong because we switched from 255 based system to normalized Floats"

        chroma =
            (1 - abs (2 * lightness - 1)) * saturation

        normHue =
            hue / degrees 60

        x =
            chroma * (1 - abs (fmod normHue 2 - 1))

        ( r, g, b ) =
            if normHue < 0 then
                ( 0, 0, 0 )
            else if normHue < 1 then
                ( chroma, x, 0 )
            else if normHue < 2 then
                ( x, chroma, 0 )
            else if normHue < 3 then
                ( 0, chroma, x )
            else if normHue < 4 then
                ( 0, x, chroma )
            else if normHue < 5 then
                ( x, 0, chroma )
            else if normHue < 6 then
                ( chroma, 0, x )
            else
                ( 0, 0, 0 )

        m =
            lightness - chroma / 2
    in
    { red = r + m
    , green = g + m
    , blue = b + m
    , alpha = alpha
    }



-- function hslToRgb(h, s, l) {
--     var r, g, b;
--     h = bound01(h, 360);
--     s = bound01(s, 100);
--     l = bound01(l, 100);
--     function hue2rgb(p, q, t) {
--         if(t < 0) t += 1;
--         if(t > 1) t -= 1;
--         if(t < 1/6) return p + (q - p) * 6 * t;
--         if(t < 1/2) return q;
--         if(t < 2/3) return p + (q - p) * (2/3 - t) * 6;
--         return p;
--     }
--     if(s === 0) {
--         r = g = b = l; // achromatic
--     }
--     else {
--         var q = l < 0.5 ? l * (1 + s) : l + s - l * s;
--         var p = 2 * l - q;
--         r = hue2rgb(p, q, h + 1/3);
--         g = hue2rgb(p, q, h);
--         b = hue2rgb(p, q, h - 1/3);
--     }
--     return { r: r * 255, g: g * 255, b: b * 255 };
-- }


fmod : Float -> Int -> Float
fmod f n =
    let
        integer =
            floor f
    in
    toFloat (integer % n) + f - toFloat integer


{-| -}
gradient : (Color -> Color -> Float -> Color) -> List Color -> List Color
gradient fn colors =
    let
        pairs =
            List.map3 (,,) (List.range 1 (List.length colors)) colors (List.drop 1 colors)

        interpolatePairs ( index, colorOne, colorTwo ) colors =
            List.foldr (\i -> interp fn colorOne colorTwo (toFloat i / 100)) [] (List.range 0 100) ++ colors
    in
    List.foldr interpolatePairs [] pairs


interp : (a -> b -> c -> d) -> a -> b -> c -> List d -> List d
interp fn colorOne colorTwo factor colors =
    fn colorOne colorTwo factor :: colors


{-| When mixing two colors in the browser, we generally want to use something called linearRgb, which accounts for the exponential method that is used to store computer colors.
-}
mix : Color -> Color -> Float -> Color
mix one two factor =
    let
        average x y =
            sqrt ((x ^ 2) * (1 - factor) + (y ^ 2) * factor)
    in
    rgba
        (average one.red two.red)
        (average one.green two.green)
        (average one.blue two.blue)
        (average one.alpha two.alpha)


{-| Get the relative luminance of a color represented as a Float.
Formula based on:

    <https://www.w3.org/TR/WCAG20/#relativeluminancedef>
    luminance Color.black -- 0.0
    luminance Color.white -- 1.0

-}
luminance : Color -> Float
luminance { red, green, blue } =
    let
        ( r, g, b ) =
            ( f red, f green, f blue )

        f intensity =
            let
                srgb =
                    intensity / 255
            in
            if srgb <= 0.03928 then
                srgb / 12.92
            else
                ((srgb + 0.055) / 1.055) ^ 2.4
    in
    0.2126 * r + 0.7152 * g + 0.0722 * b


{-| Get the contrast ratio of two colors represented as a Float.
Formula based on:
<https://www.w3.org/TR/WCAG20/#contrast-ratiodef>

    contrastRatio Color.black Color.white -- 21.0
    contrastRatio Color.blue Color.blue -- 1.0

-}
contrast : Color -> Color -> Float
contrast c1 c2 =
    let
        a =
            luminance c1 + 0.05

        b =
            luminance c2 + 0.05
    in
    if a > b then
        a / b
    else
        b / a



{- Cube Helix -}
-- {-
--    return function(a, b) {
--          a = d3.hsl(a);
--          b = d3.hsl(b);
--          var ah = (a.h + 120) * radians,
--              bh = (b.h + 120) * radians - ah,
--              as = a.s,
--              bs = b.s - as,
--              al = a.l,
--              bl = b.l - al;
--          if (isNaN(bs)) bs = 0, as = isNaN(as) ? b.s : as;
--          if (isNaN(bh)) bh = 0, ah = isNaN(ah) ? b.h : ah;
--          return function(t) {
--            var h = ah + bh * t,
--                l = Math.pow(al + bl * t, γ),
--                a = (as + bs * t) * l * (1 - l),
--                cosh = Math.cos(h),
--                sinh = Math.sin(h);
--            return "#"
--                + hex(l + a * (-0.14861 * cosh + 1.78277 * sinh))
--                + hex(l + a * (-0.29227 * cosh - 0.90649 * sinh))
--                + hex(l + a * (+1.97294 * cosh));
--          };
--        };
-- -}
-- radians =
--     pi / 180
-- wrap x y =
--     if y > x then
--         y - x
--     else if y < 0 then
--         x + y
--     else
--         y
-- cubeHelixOLD : Color.Color -> Color.Color -> Int -> Color.Color
-- cubeHelixOLD colorOne colorTwo cent =
--     let
--         factor =
--             toFloat cent / 100
--         one =
--             Color.toHsl colorOne
--         two =
--             Color.toHsl colorTwo
--         ah =
--             (one.hue + 120) * radians
--         --+ (0.3333 * pi)
--         bh =
--             -- (two.hue + (0.3333 * pi)) - ah
--             (two.hue + 120) * radians - ah
--         as1 =
--             one.saturation
--         bs =
--             two.saturation - as1
--         al =
--             one.lightness
--         bl =
--             two.lightness - al
--         h =
--             ah + bh * factor
--         gamma =
--             1.5
--         l =
--             (al + bl * factor) ^ gamma
--         a =
--             (as1 + bs * factor) * l * (1 - l)
--         cosh =
--             cos h
--         sinh =
--             sin h
--     in
--     Color.rgb
--         (round <| (*) 255 <| (l + a * (-0.14861 * cosh + 1.78277 * sinh)))
--         (round <| (*) 255 <| (l + a * (-0.29227 * cosh - 0.90649 * sinh)))
--         (round <| (*) 255 <| (l + a * (1.97294 * cosh)))
-- cubeHelix : Color.Color -> Color.Color -> Int -> Color.Color
-- cubeHelix colorOne colorTwo cent =
--     let
--         factor =
--             toFloat cent / 100
--         one =
--             Color.toHsl colorOne
--         two =
--             Color.toHsl colorTwo
--         ah =
--             one.hue + degrees 120
--         bh =
--             (two.hue + degrees 120) - ah
--         as1 =
--             one.saturation
--         bs =
--             two.saturation
--                 - as1
--         al =
--             one.lightness
--         bl =
--             two.lightness - al
--         h =
--             ah + bh * factor
--         gamma =
--             1.5
--         l =
--             (al + bl * factor) ^ gamma
--         a =
--             (as1 + bs * factor) * l * (1 - l)
--         cosh =
--             cos h
--         sinh =
--             sin h
--         channel x =
--             if x < 0 then
--                 0
--             else
--                 round (x * 255)
--     in
--     Debug.log "cube" <|
--         Color.rgb
--             (channel (l + a * (-0.14861 * cosh + 1.78277 * sinh)))
--             (channel (l + a * (-0.29227 * cosh - 0.90649 * sinh)))
--             (channel (l + a * (1.97294 * cosh)))
-- -- a =
-- --     -0.14861
-- -- b =
-- --     1.78277
-- -- c =
-- --     -0.29227
-- -- d =
-- --     -0.90649
-- -- e =
-- --     1.97294
-- -- ed =
-- --     e * d
-- -- eb =
-- --     e * b
-- -- bc_da =
-- --     b * c - d * a
-- -- cubeHelixReimplemented : Color.Color -> Color.Color -> Int -> Color.Color
-- -- cubeHelixReimplemented colorOne colorTwo cent =
-- --     let
-- --         factor =
-- --             toFloat cent / 100
-- --         one =
-- --             Color.toHsl colorOne
-- --         two =
-- --             Color.toHsl colorTwo
-- --     in
-- --     Color.rgb
-- --         (round <| (*) 255 <| (l + a * (-0.14861 * cosh + 1.78277 * sinh)))
-- --         (round <| (*) 255 <| (l + a * (-0.29227 * cosh - 0.90649 * sinh)))
-- --         (round <| (*) 255 <| (l + a * (1.97294 * cosh)))


type Vision
    = Normal


{-| Simulate common color blindness.
-}
simulate : Vision -> Color -> Color
simulate vision color =
    case vision of
        Normal ->
            color
