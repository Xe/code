package forms

// LoginForm is a struct that represents a login form for a website.
type LoginForm struct {
	Username string `form:"username" binding:"required"`
	Password string `form:"password" binding:"required"`
}
