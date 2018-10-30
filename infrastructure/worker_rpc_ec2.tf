//########################################################################
//##        Create a new Custom Backend EC2 Instance              ##
//##################### Sizes and Prices #################################
//
//resource "aws_instance" "grpc_vps_server_master" {
//  name = "CustomBackend_Server_Master"
//
//  //  name              = "${random_uuid.CustomBackend.result}"
//  availability_zone = "us-east-1b"
//  ami               = "ami-03998867" // 18.04
//
//  //  ami           = "ami-26c43149" 16.04
//  instance_type   = "t2.nano"
//  security_groups = ["${aws_security_group.swarm.name}"]
//  key_name        = "${aws_key_pair.grpc_vps_servers_key_pair.name}"
//
//  //  role            = "${aws_iam_role.iam_for_kamworks.arn}"
//
//  provisioner "file" {
//    source      = "infrastructure/nginx"
//    destination = "/home/ubuntu/nginx"
//  }
//  provisioner "file" {
//    source      = "infrastructure/docker-compose.yml"
//    destination = "/home/ubuntu/docker-compose.yml"
//  }
//  connection {
//    user        = "ubuntu"
//    private_key = "${file("infrastructure/lightsail_keys/grpc_vps_servers_key")}"
//    type        = "ssh"
//    host        = "${self.public_ip}"
//    timeout     = "20s"
//  }
//  provisioner "remote-exec" {
//    inline = [
//      "curl -sSL get.docker.com | sh",
//      "sudo usermod -aG docker $USER",
//      "sudo apt install docker-compose -y",
//
//      //      "curl -L https://github.com/docker/compose/releases/download/${var.docker_compose_version}/docker-compose-`uname -s`-`uname -m` -o /usr/local/bin/docker-compose",
//      //      "sudo chmod +x /usr/local/bin/docker-compose",
//      "sudo docker swarm init",
//
//      "sudo docker swarm join-token --quiet worker > /home/ubuntu/token",
//    ]
//
//    //      "sudo docker run -d --net=host -e 'CONSUL_LOCAL_CONFIG={\"skip_leave_on_interrupt\": true}' --name=CustomBackendConsul consul agent -server -ui -bind=${self.public_ip} -retry-join=${self.private_ip} -bootstrap-expect=${var.lightsail_worker_count}",
//  }
//  tags = {
//    Name = "CustomBackend_Swarm_Master"
//  }
//}
//
//resource "aws_instance" "grpc_vps_slave_servers" {
//  count = "${var.lightsail_worker_count}"
//  name  = "CustomBackend_Server_Slave-number${count.index}"
//
//  //  name              = "${random_uuid.CustomBackend.result}"
//  availability_zone = "us-east-1b"
//  ami               = "ami-03998867" // 18.04
//
//  //  ami           = "ami-26c43149"
//  instance_type   = "t2.nano"
//  security_groups = ["${aws_security_group.swarm.name}"]
//  key_name        = "${aws_key_pair.grpc_vps_servers_key_pair.name}"
//
//  //  role            = "${aws_iam_role.iam_for_kamworks.arn}"
//
//  provisioner "file" {
//    source      = "infrastructure/lightsail_keys/grpc_vps_servers_key"
//    destination = "/home/ubuntu/grpc_vps_servers_key.pem"
//  }
//  connection {
//    user        = "ubuntu"
//    private_key = "${file("infrastructure/lightsail_keys/grpc_vps_servers_key")}"
//    type        = "ssh"
//    host        = "${self.public_ip}"
//    timeout     = "20s"
//  }
//  provisioner "remote-exec" {
//    inline = [
//      "curl -sSL get.docker.com | sh",
//      "usermod -aG docker $USER",
//      "sudo chmod 400 /home/ubuntu/grpc_vps_servers_key.pem",
//      "sudo scp -o StrictHostKeyChecking=no -o NoHostAuthenticationForLocalhost=yes -o UserKnownHostsFile=/dev/null -i grpc_vps_servers_key.pem ubuntu@${aws_instance.grpc_vps_server_master.private_ip}:/home/ubuntu/token .",
//      "sudo docker swarm join --token $(cat /home/ubuntu/token) ${aws_instance.grpc_vps_server_master.private_ip}:2377",
//    ]
//
//    //      "sudo docker run -d --net=host -e 'CONSUL_LOCAL_CONFIG={\"leave_on_terminate\": true}' consul agent -bind=${self.public_ip} -retry-join=${aws_instance.grpc_vps_server_master.public_ip}",
//  }
//  # drain worker on destroy
//  provisioner "remote-exec" {
//    when = "destroy"
//
//    inline = [
//      "sudo docker node update --availability drain ${self.name}",
//    ]
//
//    on_failure = "continue"
//
//    connection {
//      user        = "ubuntu"
//      private_key = "${file("infrastructure/lightsail_keys/grpc_vps_servers_key")}"
//      type        = "ssh"
//      host        = "${aws_instance.grpc_vps_server_master.public_ip}"
//      timeout     = "20s"
//    }
//  }
//  # leave swarm on destroy
//  provisioner "remote-exec" {
//    when = "destroy"
//
//    inline = [
//      "sudo docker swarm leave",
//    ]
//
//    on_failure = "continue"
//  }
//  # remove node on destroy
//  provisioner "remote-exec" {
//    when = "destroy"
//
//    inline = [
//      "sudo docker node rm --force ${self.name}",
//    ]
//
//    on_failure = "continue"
//
//    connection {
//      user        = "ubuntu"
//      private_key = "${file("infrastructure/lightsail_keys/grpc_vps_servers_key")}"
//      type        = "ssh"
//      host        = "${aws_instance.grpc_vps_server_master.public_ip}"
//      timeout     = "20s"
//    }
//  }
//  tags = {
//    Name = "CustomBackend_Swarm_Slave-number${count.index}"
//  }
//}
//
///* Default security group */
//resource "aws_security_group" "swarm" {
//  name        = "swarm-group"
//  description = "Default security group that allows inbound and outbound traffic from all instances in the VPC"
//
//  ingress {
//    from_port   = "0"
//    to_port     = "0"
//    protocol    = "-1"
//    cidr_blocks = ["0.0.0.0/0"]
//    self        = true
//  }
//
//  ingress {
//    from_port   = 22
//    to_port     = 22
//    protocol    = "tcp"
//    cidr_blocks = ["0.0.0.0/0"]
//  }
//
//  egress {
//    from_port   = "0"
//    to_port     = "0"
//    protocol    = "-1"
//    cidr_blocks = ["0.0.0.0/0"]
//    self        = true
//  }
//
//  egress {
//    from_port   = 22
//    to_port     = 22
//    protocol    = "tcp"
//    cidr_blocks = ["0.0.0.0/0"]
//  }
//
//  tags {
//    Name = "CustomBackend_Swarm"
//  }
//}
//
//# Create a new Lightsail Key Pair
//resource "aws_key_pair" "grpc_vps_servers_key_pair" {
//  name       = "importing"
//  public_key = "${file("infrastructure/lightsail_keys/grpc_vps_servers_key.pub")}"
//}
//
////resource "random_uuid" "CustomBackend" {}
//
//output "master_ec2_public_ip" {
//  value = "${aws_instance.grpc_vps_server_master.public_ip}"
//}
//
//output "slaves_ec2_public_ip" {
//  value = "${aws_instance.grpc_vps_slave_servers.*.public_ip}"
//}

