
import {ControlKamClient} from './grpc/ctrlkm_grpc_web_pb';
import {UserCredentials, AuthClaims} from './grpc/ctrlkm_pb';

let client = new ControlKamClient('localhost:1991', {}, {});
let uc = new UserCredentials();
let ac = new AuthClaims();


client.login(uc, {}, (err, resp) => {
    ac = resp;
});
