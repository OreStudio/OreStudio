/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
#ifndef ORES_VAULT_CORE_SERVICE_CUSTODY_SERVICE_HPP
#define ORES_VAULT_CORE_SERVICE_CUSTODY_SERVICE_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.vault.core/export.hpp"
#include "ores.vault.core/security/challenge_registry.hpp"
#include "ores.vault.core/security/credential_source.hpp"
#include <cstdint>
#include <string>

namespace ores::vault::service {

/**
 * @brief The answer of one custody act.
 *
 * One shape for all five operations, because the handler's job is only to turn
 * one of these into the message the caller expects. A field a verb does not use
 * stays empty or zero.
 */
struct ORES_VAULT_CORE_EXPORT custody_result {
    bool success = false;
    std::string message;

    /// The secret's name.
    std::string name;

    /// The key version the act touched.
    std::int32_t key_version = 0;

    /// What a resolve answers: the value, sealed to the caller's key.
    std::string sealed_value;

    /// What a describe answers beside the name.
    std::string owner;

    /// What a resolve answers: the consumer the value was sealed to.
    std::string subject;

    /// What a rotate answers: how many rows were re-wrapped.
    std::int32_t rotated = 0;

    /// What a challenge answers: the challenge the credential must name.
    std::string challenge_id;

    /// The instant the stored version was written, for a describe.
    std::chrono::system_clock::time_point written_at;
};

/**
 * @brief Custody over the stored secrets: sealing, opening, rotating and the
 * record of every act.
 *
 * The service owns the keys and the row context, and every act it performs
 * leaves a row in the use record. It decides no policy: the handler checks the
 * permission, and the tenant and the caller's subject arrive in the context the
 * handler verified. Audit columns take the context's actor, which is the
 * username the store accepts; a subject is the identity a consumer is named by,
 * and the store refuses one in an audit column.
 */
class ORES_VAULT_CORE_EXPORT custody_service {
public:
    /**
     * @brief Builds the service over one database context and one key set.
     *
     * @param ctx The request context, already scoped to the caller's tenant.
     * @param keys The keys the deployment placed for this unit.
     */
    custody_service(const ores::database::context& ctx, security::key_material keys);

    /// The vault's transport public key, Base64-encoded. Not a secret.
    [[nodiscard]] std::string public_key() const;

    /**
     * @brief Stores a new version of one secret.
     *
     * @param subject The verified subject that asked, for the record.
     * @param name The secret's name.
     * @param owner The consumer permitted to resolve it.
     * @param sealed_to_vault The value, sealed to the vault's transport key.
     */
    custody_result write(const std::string& subject,
                         const std::string& name,
                         const std::string& owner,
                         const std::string& sealed_to_vault);

    /**
     * @brief Registers a consumer's public key under its own subject.
     *
     * The row is about the caller: the name is the subject the verified token
     * carries, so this can only ever register the caller's own key.
     *
     * @param subject The verified subject of the caller's token.
     * @param public_key The caller's public key, Base64-encoded.
     */
    custody_result register_consumer(const std::string& subject, const std::string& public_key);

    /**
     * @brief Answers the metadata of one secret, and never its value.
     */
    custody_result describe(const std::string& name) const;

    /**
     * @brief Opens one secret and seals it to the caller's registered key.
     *
     * @param subject The verified subject of the caller's token, which is the
     * name of the consumer the answer is sealed to.
     */
    custody_result resolve(const std::string& subject, const std::string& name);

    /**
     * @brief Removes one secret, leaving its record behind.
     *
     * @param subject The verified subject that asked, for the record.
     */
    custody_result remove(const std::string& subject, const std::string& name);

    /**
     * @brief Re-wraps every secret of the tenant under the current key version.
     *
     * @param subject The verified subject that asked, for the record.
     */
    custody_result rotate(const std::string& subject);

    /**
     * @brief Issues the nonce a consumer answers to prove itself before any token.
     *
     * The context must be scoped to the system tenant, where the deployment's
     * own consumers and secrets live. The nonce is sealed to the consumer's
     * registered key, so only the holder of the private half can open it.
     *
     * @param registry The process's issued challenges.
     * @param consumer The consumer's name, which is the subject its token carries.
     */
    custody_result issue_challenge(security::challenge_registry& registry,
                                   const std::string& consumer);

    /**
     * @brief Redeems a challenge and answers the consumer's own secret.
     *
     * The answer is opened with the vault's transport key under a context that
     * names the consumer, the challenge and the secret, and its nonce must be the
     * one the challenge was issued with. The challenge is retired either way.
     *
     * @param registry The process's issued challenges.
     * @param consumer The consumer the challenge was issued to.
     * @param challenge_id The challenge being answered.
     * @param name The secret the consumer asks for, which it must own.
     * @param sealed_answer The nonce and the request, sealed to the vault's key.
     */
    custody_result redeem_credential(security::challenge_registry& registry,
                                     const std::string& consumer,
                                     const std::string& challenge_id,
                                     const std::string& name,
                                     const std::string& sealed_answer);

private:
    /// Appends one row to the record of uses.
    void record_use(const std::string& subject,
                    const std::string& name,
                    const std::string& outcome,
                    std::int32_t key_version) const;

    /// The context a stored value is sealed under: its row.
    [[nodiscard]] std::string
    row_context(const std::string& name, const std::string& owner, int key_version) const;

    /// The context a delivered value is sealed under: its delivery.
    [[nodiscard]] static std::string
    delivery_context(const std::string& subject, const std::string& name, int key_version);

    // Held by value: a caller may build a context for one act and pass it
    // straight in, and a reference to that temporary would dangle by the time
    // the store runs.
    ores::database::context ctx_;
    security::key_material keys_;
};

}

#endif
