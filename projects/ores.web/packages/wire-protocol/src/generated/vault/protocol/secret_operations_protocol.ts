/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * @brief Stores a new version of one secret, sealed to the vault.
 *
 * The value is sealed to the vault's transport public key, which the caller
 * reads from =vault.v1.ops.public_key=. The vault opens it with its own private
 * key, seals it again under the key encryption key and stores one row.
 */
export interface WriteSecretRequest {
    name: string;
    owner: string;
    sealed_value: string;
}

export interface WriteSecretResponse {
    success: boolean;
    message: string;
    name: string;
    key_version: number;
}

/**
 * @brief Answers the metadata of one secret, and never its value.
 *
 * The read a screen and an operator use: it says that a value exists, who may
 * resolve it and under which key version, and it answers no ciphertext.
 */
export interface DescribeSecretRequest {
    name: string;
}

export interface DescribeSecretResponse {
    success: boolean;
    message: string;
    name: string;
    owner: string;
    key_version: number;
    written_at: string;
}

/**
 * @brief Answers one secret, sealed to the caller's registered key.
 *
 * The vault chooses the key from the subject in the verified token and never
 * from the request, so a caller cannot ask for a value sealed to somebody
 * else's key. Only the owner of the secret may resolve it.
 */
export interface ResolveSecretRequest {
    name: string;
}

export interface ResolveSecretResponse {
    success: boolean;
    message: string;
    name: string;
    key_version: number;
    sealed_value: string;
}

/**
 * @brief Removes one secret.
 *
 * The use record outlives the row it describes, so the delete leaves its own row
 * in the record and nothing else behind.
 */
export interface RemoveSecretRequest {
    name: string;
}

export interface RemoveSecretResponse {
    success: boolean;
    message: string;
}

/**
 * @brief Re-wraps every secret of the tenant under a new key version.
 *
 * One audited act: every stored value is opened under the version that sealed it
 * and sealed again under the new one, and no plaintext leaves the process. A run
 * that fails part way leaves the rows it has not reached under the old version,
 * which is why every row records its own version.
 */
export interface RotateSecretsRequest {}

export interface RotateSecretsResponse {
    success: boolean;
    message: string;
    rotated: number;
    key_version: number;
}

/**
 * @brief Answers the transport public key a caller seals a write to.
 *
 * A public key is not a secret and this read needs no permission beyond a
 * session: a caller cannot seal a value to the vault without it.
 */
export interface VaultPublicKeyRequest {}

export interface VaultPublicKeyResponse {
    success: boolean;
    message: string;
    public_key: string;
}

export const subjects = {
    write_secret_request: 'vault.v1.ops.write',
    describe_secret_request: 'vault.v1.ops.describe',
    resolve_secret_request: 'vault.v1.ops.resolve',
    remove_secret_request: 'vault.v1.ops.delete',
    rotate_secrets_request: 'vault.v1.ops.rotate',
    vault_public_key_request: 'vault.v1.ops.public_key',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    write_secret_request: true,
    describe_secret_request: true,
    resolve_secret_request: true,
    remove_secret_request: true,
    rotate_secrets_request: true,
    vault_public_key_request: true,
} as const;
