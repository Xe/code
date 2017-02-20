package duke

import "testing"

func TestGetFingerprint(t *testing.T) {
	initial := "AAAAB3NzaC1yc2EAAAADAQABAAABAQDTDQXzFma3+C43Wygo6YQnhL9gC5jIWTS+gWC7xxjX+WKStHTRwksXv1rOFuZzdmGp/iKGF1Nf2HKjMvxXgidMdfLZ642nDuNXSmTgd9qiLdLAfaYOveFPgWYnk9glGudfHMtB1KNnJvxzgXf4O+9eF0q9v2cOQJHefDAKq03yDhJ27ERN+KU/dIAI/b1FF4OZxw9YyF60lD/ySoH5hwIpi2A0H/1D9Usku1GFXzxQ8TMWfCWM47VhhpQ0+LgEKXbYa61Mblqz1MTWQYWVGT5qjx1EhA+pfsaka3H9J0OPgvrOHh1lEJTMWWDX3BfUSY41iikZr6uutIzpXGNZ6K91"
	result := "a9:b9:b1:1a:95:32:9f:82:ab:d5:b3:ca:e3:8b:56:a5"
	val := getFingerprint(initial)

	if result != val {
		t.Fatalf("Expected %q got %q", result, val)
	}
}

func TestAddColons(t *testing.T) {
	initial := "dd3bb82e850406e9abffa80ac0046ed6"
	result := "dd:3b:b8:2e:85:04:06:e9:ab:ff:a8:0a:c0:04:6e:d6"
	val := addColons(initial)

	if result != val {
		t.Fatalf("Expected %q got %q", result, val)
	}
}
