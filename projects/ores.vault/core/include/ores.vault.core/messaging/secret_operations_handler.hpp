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
#ifndef ORES_VAULT_CORE_MESSAGING_SECRET_OPERATIONS_HANDLER_HPP
#define ORES_VAULT_CORE_MESSAGING_SECRET_OPERATIONS_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.vault.api/messaging/secret_operations_protocol.hpp"
#include "ores.vault.core/security/challenge_registry.hpp"
#include "ores.vault.core/security/credential_source.hpp"
#include "ores.vault.core/service/custody_service.hpp"
#include <expected>
#include <memory>
#include <optional>
#include <string>
#include <string_view>

namespace ores::vault::messaging {

namespace {

inline auto& custody_handler_lg() {
    static auto instance = make_logger("ores.vault.messaging.secret_operations_handler");
    return instance;
}

} // namespace

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief Answers the five custody operations and the public key a caller seals
 * to.
 *
 * Every one of them reads the caller's verified context first: the tenant comes
 * from the token and never from the message, and the subject the answer is
 * sealed to comes from the token and never from the message either.
 */
class secret_operations_handler {
public:
    secret_operations_handler(ores::nats::service::client& nats,
                              const ores::database::context& ctx,
                              std::optional<ores::security::jwt::jwt_authenticator> verifier,
                              std::shared_ptr<const security::key_material> keys)
        : nats_(nats)
        , ctx_(ctx)
        , verifier_(std::move(verifier))
        , keys_(std::move(keys)) {}

    /**
     * @brief Serves vault.v1.ops.public_key.
     *
     * A public key is not a secret, and a caller cannot seal a write without
     * it, so this is the one operation that asks for no permission beyond a
     * session.
     */
    void public_key(ores::nats::message msg) {
        // A transport callback must never let an exception escape: it would
        // take the subscription with it and leave the caller waiting.
        auto original = msg;
        try {
            public_key_impl(std::move(msg));
        } catch (const std::exception& e) {
            ORES_LOG_SEV(custody_handler_lg(), error)
                << original.subject << " failed: " << e.what();
            error_reply(nats_, original, ores::service::error_code::bad_request);
        }
    }

    void public_key_impl(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(custody_handler_lg(), msg);
        auto req_ctx_expected = make_request_context(msg);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }

        service::custody_service custody(*req_ctx_expected, *keys_);
        reply(nats_,
              msg,
              vault_public_key_response{.success = true, .public_key = custody.public_key()});
    }

    /**
     * @brief Serves vault.v1.ops.register_consumer.
     *
     * A caller registers its own key. The subject comes from the token, so this
     * can only ever write the row of the caller, which is why it asks for no
     * permission beyond a session.
     */
    void register_consumer(ores::nats::message msg) {
        // A transport callback must never let an exception escape: it would
        // take the subscription with it and leave the caller waiting.
        auto original = msg;
        try {
            register_consumer_impl(std::move(msg));
        } catch (const std::exception& e) {
            ORES_LOG_SEV(custody_handler_lg(), error)
                << original.subject << " failed: " << e.what();
            error_reply(nats_, original, ores::service::error_code::bad_request);
        }
    }

    void register_consumer_impl(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(custody_handler_lg(), msg);
        auto guarded = guard(msg, "vault::consumers:read", true);
        if (!guarded) {
            return;
        }
        auto request = decode<register_consumer_request>(msg);
        if (!request) {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        service::custody_service custody(guarded->ctx, *keys_);
        const auto outcome = custody.register_consumer(guarded->subject, request->public_key);
        reply(nats_,
              msg,
              register_consumer_response{.success = outcome.success, .message = outcome.message});
    }

    /// Serves vault.v1.ops.write.
    void write(ores::nats::message msg) {
        // A transport callback must never let an exception escape: it would
        // take the subscription with it and leave the caller waiting.
        auto original = msg;
        try {
            write_impl(std::move(msg));
        } catch (const std::exception& e) {
            ORES_LOG_SEV(custody_handler_lg(), error)
                << original.subject << " failed: " << e.what();
            error_reply(nats_, original, ores::service::error_code::bad_request);
        }
    }

    void write_impl(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(custody_handler_lg(), msg);
        auto guarded = guard(msg, "vault::secrets:write");
        if (!guarded) {
            return;
        }
        auto request = decode<write_secret_request>(msg);
        if (!request) {
            ORES_LOG_SEV(custody_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        service::custody_service custody(guarded->ctx, *keys_);
        const auto outcome =
            custody.write(guarded->subject, request->name, request->owner, request->sealed_value);
        reply(nats_,
              msg,
              write_secret_response{.success = outcome.success,
                                    .message = outcome.message,
                                    .name = outcome.name,
                                    .key_version = outcome.key_version});
    }

    /// Serves vault.v1.ops.describe.
    void describe(ores::nats::message msg) {
        // A transport callback must never let an exception escape: it would
        // take the subscription with it and leave the caller waiting.
        auto original = msg;
        try {
            describe_impl(std::move(msg));
        } catch (const std::exception& e) {
            ORES_LOG_SEV(custody_handler_lg(), error)
                << original.subject << " failed: " << e.what();
            error_reply(nats_, original, ores::service::error_code::bad_request);
        }
    }

    void describe_impl(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(custody_handler_lg(), msg);
        auto guarded = guard(msg, "vault::secrets:read");
        if (!guarded) {
            return;
        }
        auto request = decode<describe_secret_request>(msg);
        if (!request) {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        service::custody_service custody(guarded->ctx, *keys_);
        const auto outcome = custody.describe(request->name);
        reply(nats_,
              msg,
              describe_secret_response{.success = outcome.success,
                                       .message = outcome.message,
                                       .name = outcome.name,
                                       .owner = outcome.owner,
                                       .key_version = outcome.key_version,
                                       .written_at = outcome.written_at});
    }

    /// Serves vault.v1.ops.resolve.
    void resolve(ores::nats::message msg) {
        // A transport callback must never let an exception escape: it would
        // take the subscription with it and leave the caller waiting.
        auto original = msg;
        try {
            resolve_impl(std::move(msg));
        } catch (const std::exception& e) {
            ORES_LOG_SEV(custody_handler_lg(), error)
                << original.subject << " failed: " << e.what();
            error_reply(nats_, original, ores::service::error_code::bad_request);
        }
    }

    void resolve_impl(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(custody_handler_lg(), msg);
        auto guarded = guard(msg, "vault::secrets:resolve");
        if (!guarded) {
            return;
        }
        auto request = decode<resolve_secret_request>(msg);
        if (!request) {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        service::custody_service custody(guarded->ctx, *keys_);
        const auto outcome = custody.resolve(guarded->subject, request->name);
        reply(nats_,
              msg,
              resolve_secret_response{.success = outcome.success,
                                      .message = outcome.message,
                                      .name = outcome.name,
                                      .key_version = outcome.key_version,
                                      .sealed_value = outcome.sealed_value,
                                      .subject = outcome.subject});
    }

    /// Serves vault.v1.ops.delete.
    void remove(ores::nats::message msg) {
        // A transport callback must never let an exception escape: it would
        // take the subscription with it and leave the caller waiting.
        auto original = msg;
        try {
            remove_impl(std::move(msg));
        } catch (const std::exception& e) {
            ORES_LOG_SEV(custody_handler_lg(), error)
                << original.subject << " failed: " << e.what();
            error_reply(nats_, original, ores::service::error_code::bad_request);
        }
    }

    void remove_impl(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(custody_handler_lg(), msg);
        auto guarded = guard(msg, "vault::secrets:delete");
        if (!guarded) {
            return;
        }
        auto request = decode<remove_secret_request>(msg);
        if (!request) {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        service::custody_service custody(guarded->ctx, *keys_);
        const auto outcome = custody.remove(guarded->subject, request->name);
        reply(nats_,
              msg,
              remove_secret_response{.success = outcome.success, .message = outcome.message});
    }

    /// Serves vault.v1.ops.rotate.
    void rotate(ores::nats::message msg) {
        // A transport callback must never let an exception escape: it would
        // take the subscription with it and leave the caller waiting.
        auto original = msg;
        try {
            rotate_impl(std::move(msg));
        } catch (const std::exception& e) {
            ORES_LOG_SEV(custody_handler_lg(), error)
                << original.subject << " failed: " << e.what();
            error_reply(nats_, original, ores::service::error_code::bad_request);
        }
    }

    void rotate_impl(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(custody_handler_lg(), msg);
        auto guarded = guard(msg, "vault::secrets:write");
        if (!guarded) {
            return;
        }

        service::custody_service custody(guarded->ctx, *keys_);
        const auto outcome = custody.rotate(guarded->subject);
        reply(nats_,
              msg,
              rotate_secrets_response{.success = outcome.success,
                                      .message = outcome.message,
                                      .rotated = outcome.rotated,
                                      .key_version = outcome.key_version});
    }

    /**
     * @brief Serves vault.v1.ops.challenge.
     *
     * The caller holds no token: this is the exchange that makes one possible.
     * It is answered in the system tenant, where the deployment's own consumers
     * live, and it asks for nothing but a consumer the vault knows.
     */
    void challenge(ores::nats::message msg) {
        auto original = msg;
        try {
            challenge_impl(std::move(msg));
        } catch (const std::exception& e) {
            ORES_LOG_SEV(custody_handler_lg(), error)
                << original.subject << " failed: " << e.what();
            error_reply(nats_, original, ores::service::error_code::bad_request);
        }
    }

    void challenge_impl(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(custody_handler_lg(), msg);
        auto request = decode<challenge_request>(msg);
        if (!request) {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        service::custody_service custody(system_context(), *keys_);
        const auto outcome = custody.issue_challenge(challenges_, request->consumer);
        reply(nats_,
              msg,
              challenge_response{.success = outcome.success,
                                 .message = outcome.message,
                                 .challenge_id = outcome.challenge_id,
                                 .nonce = outcome.sealed_value});
    }

    /**
     * @brief Serves vault.v1.ops.credential.
     *
     * The answer must open the challenge the vault issued to that consumer, and
     * the secret it asks for must be one the consumer owns. It is the same
     * exchange a token buys, narrower: the caller may read its own entry and no
     * other.
     */
    void credential(ores::nats::message msg) {
        auto original = msg;
        try {
            credential_impl(std::move(msg));
        } catch (const std::exception& e) {
            ORES_LOG_SEV(custody_handler_lg(), error)
                << original.subject << " failed: " << e.what();
            error_reply(nats_, original, ores::service::error_code::bad_request);
        }
    }

    void credential_impl(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(custody_handler_lg(), msg);
        auto request = decode<credential_request>(msg);
        if (!request) {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        service::custody_service custody(system_context(), *keys_);
        const auto outcome = custody.redeem_credential(challenges_,
                                                       request->consumer,
                                                       request->challenge_id,
                                                       request->name,
                                                       request->sealed_answer);
        reply(nats_,
              msg,
              credential_response{.success = outcome.success,
                                  .message = outcome.message,
                                  .name = outcome.name,
                                  .key_version = outcome.key_version,
                                  .sealed_value = outcome.sealed_value});
    }

private:
    /// The deployment's own tenant, where its consumers and their secrets live.
    [[nodiscard]] ores::database::context system_context() const {
        return ores::database::service::tenant_context::with_system_tenant(ctx_);
    }

    /// What a guarded request yields: the scoped context and the verified
    /// subject, which is the consumer the vault names.
    struct guarded_request {
        ores::database::context ctx;
        std::string subject;
    };

    [[nodiscard]] std::expected<ores::database::context, ores::service::error_code>
    make_request_context(const ores::nats::message& msg) const {
        return ores::service::service::make_request_context(ctx_, msg, verifier_);
    }

    /**
     * @brief Checks the caller holds a permission, and reads its subject.
     *
     * The subject is read from the same token the context was built from, so
     * the identity the vault seals to is the identity the store authenticated.
     */
    std::optional<guarded_request> guard(const ores::nats::message& msg,
                                         const std::string& permission,
                                         bool session_only = false) {
        auto ctx_expected = make_request_context(msg);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return std::nullopt;
        }
        // A token whose act names an agent is refused here as well as at the
        // token's making: no agent reads a secret, whatever its role holds.
        if (ctx_expected->acts_for_agent()) {
            ORES_LOG_SEV(custody_handler_lg(), warn) << "An agent asked the vault: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return std::nullopt;
        }
        if (!session_only && !has_permission(*ctx_expected, permission)) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return std::nullopt;
        }

        const auto token = bearer_token(msg);
        if (!verifier_.has_value() || token.empty()) {
            ORES_LOG_SEV(custody_handler_lg(), warn) << "No token on a custody request";
            error_reply(nats_, msg, ores::service::error_code::unauthorized);
            return std::nullopt;
        }
        const auto claims = verifier_->validate(token);
        if (!claims) {
            error_reply(nats_, msg, ores::service::error_code::unauthorized);
            return std::nullopt;
        }

        return guarded_request{.ctx = *ctx_expected, .subject = claims->subject};
    }

    /// The bearer token a message carries, or nothing when it carries none.
    [[nodiscard]] static std::string bearer_token(const ores::nats::message& msg) {
        const auto found = msg.headers.find(std::string(ores::nats::headers::authorization));
        if (found == msg.headers.end()) {
            return {};
        }
        const auto& value = found->second;
        constexpr std::string_view prefix = "Bearer ";
        if (value.rfind(prefix, 0) == 0) {
            return value.substr(prefix.size());
        }
        return value;
    }

    ores::nats::service::client& nats_;
    // Held by value: the context the caller registered with dies with that call,
    // so a reference to it would dangle by the time a request arrives.
    ores::database::context ctx_;
    security::challenge_registry challenges_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    std::shared_ptr<const security::key_material> keys_;
};

}

#endif
