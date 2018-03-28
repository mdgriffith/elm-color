# A Common Color

The `Color` type is moving out of core (which makes a lot of sense) in `0.19`, so I wanted to discuss what that means for libraries that use it.

The main benefit of `Color` being in the core library was having a single type that color libraries could use to work with each other.

So, let's just standardize over one color type! Here's what I think it should be:

```elm
type alias Color =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }
```

Each channel is normalized between 0 and 1.


This package is meant to be a series of convenience functions around this new color type.

The cool part being you can be compatible with that library without having to depend on it.

It uses that type as it's `Color` and includes functions for:

- converting hsl to standard color (i.e. rgb)
- calculating contrast of colors
- simulating vision characteristics (color blindness, etc.)
- mixing two colors using the linearRgb colorspace.
- formatting 


Both `elm-style-animation` and `style-elements` need to use a `Color` type, but before making that change I wanted to post this here :)



