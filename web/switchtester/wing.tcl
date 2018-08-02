proc wingcss {} {
    wapp-trim {
        <link rel="stylesheet" href="/wing.css" />
    }
}

proc wapp-page-wing.css {} {
    wapp-mimetype text/css
    wapp-cache-control max-age=3600
    wapp-unsafe [binary decode base64 {
      LyoNCiogV2luZyAxLjAuMC1iZXRhDQoqIENvcHlyaWdodCAyMDE2LCBLYWJpciBTaGFoDQoqIGh0
      dHA6Ly91c2V3aW5nLm1sLw0KKiBGcmVlIHRvIHVzZSB1bmRlciB0aGUgTUlUIGxpY2Vuc2UuDQoq
      IGh0dHBzOi8va2luZ3BpeGlsLmdpdGh1Yi5pby9saWNlbnNlDQoqLw0KYm9keSxoMSxoMixoMyxo
      NCxoNSxoNntmb250LXdlaWdodDo0MDA7Zm9udC1mYW1pbHk6LWFwcGxlLXN5c3RlbSxCbGlua01h
      Y1N5c3RlbUZvbnQsQXZlbmlyLCJBdmVuaXIgTmV4dCIsIlNlZ29lIFVJIixSb2JvdG8sT3h5Z2Vu
      LFVidW50dSxDYW50YXJlbGwsIkZpcmEgU2FucyIsIkRyb2lkIFNhbnMiLCJIZWx2ZXRpY2EgTmV1
      ZSIsc2Fucy1zZXJpZn1ib2R5LGg2e2xpbmUtaGVpZ2h0OjEuNn1odG1se2JveC1zaXppbmc6Ym9y
      ZGVyLWJveDtmb250LXNpemU6NjIuNSU7bWFyZ2luOjA7cGFkZGluZzowfWJvZHl7bGV0dGVyLXNw
      YWNpbmc6LjAxZW07Zm9udC1zaXplOjEuNWVtfWgxLGgyLGgze2xldHRlci1zcGFjaW5nOi0uMXJl
      bX1oMXtmb250LXNpemU6NHJlbTtsaW5lLWhlaWdodDoxLjJ9aDJ7Zm9udC1zaXplOjMuNnJlbTts
      aW5lLWhlaWdodDoxLjI1fWgze2ZvbnQtc2l6ZTozcmVtO2xpbmUtaGVpZ2h0OjEuM31oNHtmb250
      LXNpemU6Mi40cmVtO2xpbmUtaGVpZ2h0OjEuMzU7bGV0dGVyLXNwYWNpbmc6LS4wOHJlbX1oNXtm
      b250LXNpemU6MS44cmVtO2xpbmUtaGVpZ2h0OjEuNTtsZXR0ZXItc3BhY2luZzotLjA1cmVtfWg2
      e2ZvbnQtc2l6ZToxLjVyZW07bGV0dGVyLXNwYWNpbmc6MH1AbWVkaWEgKG1pbi13aWR0aDo1NTBw
      eCl7aDF7Zm9udC1zaXplOjVyZW19aDJ7Zm9udC1zaXplOjQuMnJlbX1oM3tmb250LXNpemU6My42
      cmVtfWg0e2ZvbnQtc2l6ZTozcmVtfWg1e2ZvbnQtc2l6ZToyLjRyZW19aDZ7Zm9udC1zaXplOjEu
      NXJlbX19YXtjb2xvcjojMTA0Y2ZiO3RyYW5zaXRpb246YWxsIC4xcyBlYXNlfWE6aG92ZXJ7Y29s
      b3I6IzIyMn1bdHlwZT1zdWJtaXRdLGJ1dHRvbntwYWRkaW5nOjEuMXJlbSAzLjVyZW07bWFyZ2lu
      OjFyZW0gMDtiYWNrZ3JvdW5kOiMxMTE7Y29sb3I6I2Y1ZjVmNTtib3JkZXItcmFkaXVzOjJweDti
      b3JkZXI6bm9uZTtmb250LXNpemU6MS4zcmVtO3RyYW5zaXRpb246YWxsIC4ycyBlYXNlfVt0eXBl
      PXN1Ym1pdF0ub3V0bGluZSxidXR0b24ub3V0bGluZXtwYWRkaW5nOjEuMXJlbSAzLjVyZW07YmFj
      a2dyb3VuZDowIDA7Y29sb3I6IzExMTtib3JkZXI6MS41cHggc29saWQgIzExMX1bdHlwZT1zdWJt
      aXRdOmhvdmVyLGJ1dHRvbjpob3ZlcntiYWNrZ3JvdW5kOiMyMjJ9W3R5cGU9c3VibWl0XS5vdXRs
      aW5lOmhvdmVyLGJ1dHRvbi5vdXRsaW5lOmhvdmVye2JhY2tncm91bmQ6MCAwO2JvcmRlcjoxLjVw
      eCBzb2xpZCAjNDQ0O2NvbG9yOiM0NDR9W3R5cGU9c3VibWl0XTpmb2N1cyxidXR0b246Zm9jdXN7
      b3V0bGluZTowfVt0eXBlPXN1Ym1pdF06YWN0aXZlLGJ1dHRvbjphY3RpdmV7dHJhbnNmb3JtOnNj
      YWxlKC45OSl9aW5wdXRbdHlwZT1lbWFpbF0saW5wdXRbdHlwZT1maWxlXSxpbnB1dFt0eXBlPW51
      bWJlcl0saW5wdXRbdHlwZT1wYXNzd29yZF0saW5wdXRbdHlwZT1zZWFyY2hdLGlucHV0W3R5cGU9
      dGVsXSxpbnB1dFt0eXBlPXRleHRdLHNlbGVjdCx0ZXh0YXJlYSx0ZXh0YXJlYVt0eXBlPXRleHRd
      e21hcmdpbjoxcmVtIDA7d2lkdGg6MTAwJTttYXgtd2lkdGg6MTAwJTtib3JkZXItcmFkaXVzOjJw
      eDtib3JkZXI6MXB4IHNvbGlkICNhNGE0YTQ7Zm9udC1zaXplOjEuM3JlbTt0cmFuc2l0aW9uOmFs
      bCAuMnMgZWFzZX1pbnB1dFt0eXBlPWVtYWlsXTpob3ZlcixpbnB1dFt0eXBlPWZpbGVdLGlucHV0
      W3R5cGU9bnVtYmVyXTpob3ZlcixpbnB1dFt0eXBlPXBhc3N3b3JkXTpob3ZlcixpbnB1dFt0eXBl
      PXNlYXJjaF06aG92ZXIsaW5wdXRbdHlwZT10ZWxdLGlucHV0W3R5cGU9dGV4dF06aG92ZXIsc2Vs
      ZWN0OmhvdmVyLHRleHRhcmVhOmhvdmVyLHRleHRhcmVhW3R5cGU9dGV4dF06aG92ZXJ7Ym9yZGVy
      OjFweCBzb2xpZCAjMTExfWlucHV0W3R5cGU9ZW1haWxdOmZvY3VzLGlucHV0W3R5cGU9ZmlsZV0s
      aW5wdXRbdHlwZT1udW1iZXJdLGlucHV0W3R5cGU9cGFzc3dvcmRdOmZvY3VzLGlucHV0W3R5cGU9
      c2VhcmNoXTpmb2N1cyxpbnB1dFt0eXBlPXRlbF0saW5wdXRbdHlwZT10ZXh0XTpmb2N1cyxzZWxl
      Y3Q6Zm9jdXMgdGV4dGFyZWE6Zm9jdXMsdGV4dGFyZWFbdHlwZT10ZXh0XTpmb2N1c3tvdXRsaW5l
      OjA7Ym9yZGVyOjFweCBzb2xpZCAjMTA0Y2ZifWlucHV0W3R5cGU9ZW1haWxdLGlucHV0W3R5cGU9
      ZmlsZV0saW5wdXRbdHlwZT1udW1iZXJdLGlucHV0W3R5cGU9cGFzc3dvcmRdLGlucHV0W3R5cGU9
      c2VhcmNoXSxpbnB1dFt0eXBlPXRlbF0saW5wdXRbdHlwZT10ZXh0XSxzZWxlY3R7cGFkZGluZzox
      LjFyZW19dGV4dGFyZWEsdGV4dGFyZWFbdHlwZT10ZXh0XXtoZWlnaHQ6MTByZW07cGFkZGluZzox
      NHB4IDIwcHh9LmNvbnRhaW5lcnttYXgtd2lkdGg6OTYwcHg7bWFyZ2luOjAgYXV0bzt3aWR0aDo4
      MCV9LnJvd3tkaXNwbGF5OmZsZXg7ZmxleC1mbG93OnJvdyB3cmFwO2p1c3RpZnktY29udGVudDpz
      cGFjZS1iZXR3ZWVufS5yb3c+OmZpcnN0LWNoaWxke21hcmdpbi1sZWZ0OjB9LnJvdz46bGFzdC1j
      aGlsZHttYXJnaW4tcmlnaHQ6MH0uY29sey13ZWJraXQtYm94LWZsZXg6MTstbW96LWJveC1mbGV4
      OjE7LXdlYmtpdC1mbGV4OjE7LW1zLWZsZXg6MTtmbGV4OjF9LmNvbCxbY2xhc3MqPSIgY29sLSJd
      LFtjbGFzc149Y29sLV17bWFyZ2luOjFyZW19LmNvbC0xe2ZsZXg6MX0uY29sLTJ7ZmxleDoyfS5j
      b2wtM3tmbGV4OjN9LmNvbC00e2ZsZXg6NH0uY29sLTV7ZmxleDo1fS5jb2wtNntmbGV4OjZ9LmNv
      bC03e2ZsZXg6N30uY29sLTh7ZmxleDo4fS5jb2wtOXtmbGV4Ojl9LmNvbC0xMHtmbGV4OjEwfS5j
      b2wtMTF7ZmxleDoxMX0uY29sLTEye2ZsZXg6MTJ9dWx7bGlzdC1zdHlsZTpjaXJjbGUgaW5zaWRl
      fW9se2xpc3Qtc3R5bGU6ZGVjaW1hbCBpbnNpZGV9LnRhYmxle3dpZHRoOjEwMCU7Ym9yZGVyOm5v
      bmU7Ym9yZGVyLWNvbGxhcHNlOmNvbGxhcHNlO2JvcmRlci1zcGFjaW5nOjA7dGV4dC1hbGlnbjps
      ZWZ0fS50YWJsZSB0ZCwudGFibGUgdGh7dmVydGljYWwtYWxpZ246bWlkZGxlO3BhZGRpbmc6MTJw
      eCA0cHh9LnRhYmxlIHRoZWFke2JvcmRlci1ib3R0b206MnB4IHNvbGlkICMzMzMwMzB9QG1lZGlh
      IHNjcmVlbiBhbmQgKG1heC13aWR0aDo3NjhweCl7LmNvbCxbY2xhc3MqPSIgY29sLSJdLFtjbGFz
      c149Y29sLV17bWFyZ2luOjA7ZmxleDowIDAgMTAwJX0udGFibGUucmVzcG9uc2l2ZXtwb3NpdGlv
      bjpyZWxhdGl2ZTtkaXNwbGF5OmJsb2NrfS50YWJsZS5yZXNwb25zaXZlIHRkLC50YWJsZS5yZXNw
      b25zaXZlIHRoe21hcmdpbjowfS50YWJsZS5yZXNwb25zaXZlIHRoZWFke2Rpc3BsYXk6YmxvY2s7
      ZmxvYXQ6bGVmdDtib3JkZXI6MH0udGFibGUucmVzcG9uc2l2ZSB0aGVhZCB0cntkaXNwbGF5OmJs
      b2NrO3BhZGRpbmc6MCAxMHB4IDAgMDtib3JkZXItcmlnaHQ6MnB4IHNvbGlkICMzMzMwMzB9LnRh
      YmxlLnJlc3BvbnNpdmUgdGh7ZGlzcGxheTpibG9jazt0ZXh0LWFsaWduOnJpZ2h0fS50YWJsZS5y
      ZXNwb25zaXZlIHRib2R5e2Rpc3BsYXk6YmxvY2s7b3ZlcmZsb3cteDphdXRvO3doaXRlLXNwYWNl
      Om5vd3JhcH0udGFibGUucmVzcG9uc2l2ZSB0Ym9keSB0cntkaXNwbGF5OmlubGluZS1ibG9ja30u
      dGFibGUucmVzcG9uc2l2ZSB0ZHtkaXNwbGF5OmJsb2NrO21pbi1oZWlnaHQ6MTZweDt0ZXh0LWFs
      aWduOmxlZnR9LnRhYmxlLnJlc3BvbnNpdmUgdHJ7cGFkZGluZzowIDEwcHh9fS5wdWxsLXJpZ2h0
      e2Zsb2F0OnJpZ2h0fS5wdWxsLWxlZnR7ZmxvYXQ6bGVmdH0udGV4dC1jZW50ZXJ7dGV4dC1hbGln
      bjpjZW50ZXJ9LmZ1bGwtc2NyZWVue3dpZHRoOjEwMCU7bWluLWhlaWdodDoxMDB2aH0udmVydGlj
      YWwtYWxpZ257ZGlzcGxheTpmbGV4O2FsaWduLWl0ZW1zOmNlbnRlcn0uaG9yaXpvbnRhbC1hbGln
      bntkaXNwbGF5OmZsZXg7anVzdGlmeS1jb250ZW50OmNlbnRlcn0uY2VudGVyLC5yaWdodHthbGln
      bi1pdGVtczpjZW50ZXI7ZGlzcGxheTpmbGV4fS5jZW50ZXJ7anVzdGlmeS1jb250ZW50OmNlbnRl
      cn0ucmlnaHR7anVzdGlmeS1jb250ZW50OmZsZXgtZW5kfS5sZWZ0e2Rpc3BsYXk6ZmxleDthbGln
      bi1pdGVtczpjZW50ZXI7anVzdGlmeS1jb250ZW50OmZsZXgtc3RhcnR9LmZpeGVke3Bvc2l0aW9u
      OmZpeGVkO3dpZHRoOjEwMCV9QG1lZGlhIHNjcmVlbiBhbmQgKG1heC13aWR0aDo0MDBweCl7Lmhp
      ZGUtcGhvbmV7ZGlzcGxheTpub25lfX1AbWVkaWEgc2NyZWVuIGFuZCAobWF4LXdpZHRoOjc2OHB4
      KXsuaGlkZS10YWJsZXR7ZGlzcGxheTpub25lfX1jb2Rle3BhZGRpbmc6LjJyZW0gLjVyZW07bWFy
      Z2luOjAgLjJyZW07Zm9udC1zaXplOjkwJTt3aGl0ZS1zcGFjZTpub3dyYXA7YmFja2dyb3VuZDoj
      RjFGMUYxO2JvcmRlcjoxcHggc29saWQgI0UxRTFFMTtib3JkZXItcmFkaXVzOjRweDtmb250LWZh
      bWlseTpDb25zb2xhcyxNb25hY28sTWVubG8sbW9ub3NwYWNlfXByZT5jb2Rle2Rpc3BsYXk6Ymxv
      Y2s7cGFkZGluZzoxcmVtIDEuNXJlbTt3aGl0ZS1zcGFjZTpwcmUtd3JhcDt3aGl0ZS1zcGFjZTot
      bW96LXByZS13cmFwO3doaXRlLXNwYWNlOi1wcmUtd3JhcDt3aGl0ZS1zcGFjZTotby1wcmUtd3Jh
      cDt3b3JkLXdyYXA6YnJlYWstd29yZH0ubmF2LC5uYXYtYnJhbmQsLm5hdi1tZW51e2Rpc3BsYXk6
      ZmxleH0ubmF2e3Bvc2l0aW9uOnJlbGF0aXZlO2ZsZXgtd3JhcDp3cmFwO2FsaWduLWl0ZW1zOmNl
      bnRlcjtwYWRkaW5nOjFyZW19Lm5hdi1tZW51e2ZsZXgtZmxvdzpyb3c7ZmxleDoxIDAgYXV0b30u
      bmF2LWl0ZW17cGFkZGluZzoxcmVtIDJyZW19Lm5hdi1sb2dve2ZvbnQtd2VpZ2h0OmJvbGRlcjtm
      b250LXNpemU6MnJlbTtsaW5lLWhlaWdodDowfQ==
    }]
    wapp-trim {
        /* custom hax */
        body {
            background: #f5f5f5;
            color: #202020;
        }
        table {
            border: 1px solid black;
        }
    }
}
