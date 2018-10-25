export class AuthClaims {
  constructor ();
  getToken(): string;
  setToken(a: string): void;
  getUserid(): string;
  setUserid(a: string): void;
  getEmail(): string;
  setEmail(a: string): void;
  getRole(): number;
  setRole(a: number): void;
  serializeBinary(): Uint8Array;
  static deserializeBinary: (bytes: {}) => AuthClaims;
}

export class UserCredentials {
  constructor ();
  getEmail(): string;
  setEmail(a: string): void;
  getPassword(): string;
  setPassword(a: string): void;
  serializeBinary(): Uint8Array;
  static deserializeBinary: (bytes: {}) => UserCredentials;
}

