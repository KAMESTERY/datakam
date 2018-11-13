import * as grpcWeb from 'grpc-web';
import {
  AuthClaims,
  UserCredentials} from './ctrlkm_pb';

export class ControlKamClient {
  constructor (hostname: string,
               credentials: {},
               options: { [s: string]: {}; });

  login(
    request: UserCredentials,
    metadata: grpcWeb.Metadata,
    callback: (err: grpcWeb.Error,
               response: AuthClaims) => void
  ): grpcWeb.ClientReadableStream;

}

