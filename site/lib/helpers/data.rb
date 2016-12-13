module DataHelper
  def data_from(identifier)
    YAML.load_file("data/#{identifier}.yaml")
  end
end
