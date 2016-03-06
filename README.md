# youseibox, wiring between RDB and elasticsearch

## 開発時の注意

otto devによって作られるVMのメモリ量は非常に少ない。
elasticsearchを正常に動かすため、以下の設定でVMのメモリ量を増やす

1. `otto compile`
2. `.otto/compiled/app/dev/Vagrantfile`へ以下を追記
    ```ruby
      config.vm.provider "virtualbox" do |vb|
        vb.memory = "4096"
      end
    ```
