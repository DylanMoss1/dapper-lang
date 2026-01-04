fun classify(x) {
    if x <= 0 then
        0
    else
        if x <= 10 then 1 else 2
}

fun main() {
    classify(5)
}
