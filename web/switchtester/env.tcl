proc wapp-page-env {} {
    wapp-allow-xorigin-params
    wapp-trim {
        <h1>Wapp Environment</h1>
        <pre>%html([wapp-debug-env])</pre>
    }
}
