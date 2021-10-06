module Styles exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)

-- Wind and Sun meter
meterCss : List (String, String)
meterCss =  [( "width", "250px" ), ( "height", "15px" )]

-- Group toggle with bar
groupCss colorOfBlock = [ ( "background-color", colorOfBlock ), ( "width", "300px" ), ( "margin", "0px 0px 10px 0px" ), ( "padding", "10px 10px 10px 10px" ) ]

groupTextCss = [ ( "padding-left", "20px" ), ( "font-size", "120%" ) ]

{- The switch - the box around the slider -}
toggleSwitchCss = [(  "position", "relative"), ("display", "inline-block"),   ("width", "60px"), ("height", "34px")]

{- Hide default HTML checkbox -}
toggleSwitchInputCss = [("display", "none")]

{-The slider-}
toggleSliderCss = [("position", "absolute"), ("cursor", "pointer"), ("top","0"), ("left","0"),("right","0"), ("bottom","0"), ("background-color","#ccc"),("-webkit-transition", ".4s"), ("transition",  ".4s")]

{-
.slider::before {
  position: absolute;
  content: "";
  height: 26px;
  width: 26px;
  left: 4px;
  bottom: 4px;
  background-color: white;
  -webkit-transition: .4s;
  transition: .4s;
}

input:checked + .slider {
  background-color: #2196F3;
}

input:focus + .slider {
  box-shadow: 0 0 1px #2196F3;
}

input:checked + .slider:before {
  -webkit-transform: translateX(26px);
  -ms-transform: translateX(26px);
  transform: translateX(26px);
}

/* Rounded sliders */
.slider.round {
  border-radius: 34px;
}

.slider.round:before {
  border-radius: 50%;
}
-}