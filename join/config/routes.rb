Rails.application.routes.draw do
  root to: 'motivations#new'

  resources :motivations, only: [:new, :create]
  get :success, controller: :pages
end
