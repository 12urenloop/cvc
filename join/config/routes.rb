Rails.application.routes.draw do
  root to: 'motivations#new'

  resources :motivations, only: [:new, :create, :index]
  get :success, controller: :pages

  get :signature, controller: :signatures
  get :makesignature, controller: :signatures
end
