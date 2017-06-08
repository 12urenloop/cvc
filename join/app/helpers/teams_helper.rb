module TeamsHelper
  def render_team team
    render partial: "teams/#{team[:key]}", layout: 'teams/team', locals: { team: team }
  end

  def render_image src
    render layout: 'teams/image' do
      image_tag src
    end
  end
end
