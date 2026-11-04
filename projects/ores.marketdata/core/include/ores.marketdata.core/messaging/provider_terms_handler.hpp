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
#ifndef ORES_MARKETDATA_CORE_MESSAGING_PROVIDER_TERMS_HANDLER_HPP
#define ORES_MARKETDATA_CORE_MESSAGING_PROVIDER_TERMS_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/provider_terms_list_operations_protocol.hpp"
#include "ores.marketdata.api/messaging/provider_terms_operations_protocol.hpp"
#include "ores.marketdata.core/repository/provider_repository.hpp"
#include "ores.marketdata.core/terms/provider_terms.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <chrono>
#include <optional>
#include <string>

namespace ores::marketdata::messaging {

namespace {

inline auto& provider_terms_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.provider_terms_handler");
    return instance;
}

}

/**
 * @brief Records the acceptance of a provider's terms, or its withdrawal: the one writer of
 * either.
 *
 * The caller names the provider. The terms accepted are the ones the service holds, the
 * account is the session's and the time is the service's clock, so no caller can state an
 * acceptance of terms it did not see.
 */
class provider_terms_handler {
public:
    // A list accept writes one version a provider; the BFF caps its body the same way.
    static constexpr std::size_t max_providers_per_accept = 100;

    provider_terms_handler(ores::nats::service::client& nats,
                           ores::database::context ctx,
                           std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void accept(ores::nats::message msg) {
        using ores::service::messaging::error_reply;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!ores::service::messaging::has_permission(req_ctx,
                                                      "marketdata::providers:accept_terms")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = ores::service::messaging::decode<accept_provider_terms_request>(msg);
        if (!req) {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        const auto outcome = accept_one(req_ctx, req->provider_code, req->provider_version);
        const accept_provider_terms_response resp{.success = outcome.success,
                                                  .message = outcome.message,
                                                  .accepted_by = outcome.accepted_by,
                                                  .accepted_at = outcome.accepted_at};
        ores::service::messaging::reply(nats_, msg, resp);
    }

    void accept_many(ores::nats::message msg) {
        using ores::service::messaging::error_reply;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!ores::service::messaging::has_permission(req_ctx,
                                                      "marketdata::providers:accept_terms")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = ores::service::messaging::decode<accept_many_provider_terms_request>(msg);
        if (!req || req->providers.size() > max_providers_per_accept) {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        accept_many_provider_terms_response resp;
        std::size_t accepted = 0;
        for (const auto& choice : req->providers) {
            auto outcome = accept_one(req_ctx, choice.provider_code, choice.provider_version);
            if (outcome.success)
                ++accepted;
            resp.outcomes.push_back(std::move(outcome));
        }
        resp.success = accepted == resp.outcomes.size();
        resp.message = "Accepted the terms of " + std::to_string(accepted) + " of " +
                       std::to_string(resp.outcomes.size()) + " provider(s).";
        ores::service::messaging::reply(nats_, msg, resp);
    }

    void withdraw(ores::nats::message msg) {
        using namespace ores::logging;
        using ores::service::messaging::error_reply;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!ores::service::messaging::has_permission(req_ctx,
                                                      "marketdata::providers:accept_terms")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = ores::service::messaging::decode<withdraw_provider_terms_request>(msg);
        if (!req) {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        withdraw_provider_terms_response resp;
        try {
            repository::provider_repository repo;
            auto found = repo.read_latest(req_ctx, req->provider_code);
            if (found.empty()) {
                resp.message = "No provider '" + req->provider_code + "'.";
            } else if (found.front().terms_accepted_digest.empty()) {
                resp.success = true;
                resp.message = "The terms of '" + req->provider_code + "' were not accepted.";
            } else {
                const auto& actor = req_ctx.actor();
                auto withdrawn = terms::withdraw_terms(found.front());
                withdrawn.modified_by = actor;
                withdrawn.performed_by = req_ctx.service_account();
                withdrawn.change_reason_code = "system.update";
                withdrawn.change_commentary = "Withdrew the acceptance of the terms";
                repo.write(req_ctx, {withdrawn}, {claim_of(found.front())});
                resp.success = true;
                resp.message = "Withdrew the acceptance of '" + req->provider_code + "'.";
                BOOST_LOG_SEV(provider_terms_handler_lg(), info)
                    << actor << " withdrew the acceptance of provider '" << req->provider_code
                    << "'";
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(provider_terms_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        ores::service::messaging::reply(nats_, msg, resp);
    }

private:
    /**
     * @brief Accepts one provider's terms, as the single and the list accept both do.
     *
     * A provider that is unknown, states no terms or changed since it was read is
     * refused with the reason, and one whose terms are already accepted as they stand is
     * left as it is; otherwise the acceptance is written against the version read, so a
     * provider that changes in between is not accepted unseen.
     */
    static provider_terms_outcome
    accept_one(const ores::database::context& req_ctx, const std::string& code, int version) {
        provider_terms_outcome out{.provider_code = code};
        try {
            repository::provider_repository repo;
            auto found = repo.read_latest(req_ctx, code);
            if (found.empty()) {
                out.message = "No provider '" + code + "'.";
            } else if (!terms::has_terms(found.front())) {
                out.message = "The provider '" + code + "' states no terms.";
            } else if (terms::terms_accepted(found.front())) {
                // The terms as they stand are already accepted, so nothing is written and a
                // setup that runs again leaves the provider's history as it was.
                out.success = true;
                out.message = "The terms of '" + code + "' are already accepted.";
                out.accepted_by = found.front().terms_accepted_by;
                out.accepted_at = found.front().terms_accepted_at;
            } else if (version != 0 && version != found.front().version) {
                out.message = "The provider '" + code +
                              "' changed after its terms were read. Read them again.";
            } else {
                const auto& actor = req_ctx.actor();
                auto accepted =
                    terms::accept_terms(found.front(), actor, std::chrono::system_clock::now());
                accepted.modified_by = actor;
                accepted.performed_by = req_ctx.service_account();
                accepted.change_reason_code = "system.update";
                accepted.change_commentary = "Accepted the terms at " + accepted.terms_url;
                repo.write(req_ctx, {accepted}, {claim_of(found.front())});
                out.success = true;
                out.message = "Accepted the terms of '" + code + "'.";
                out.accepted_by = accepted.terms_accepted_by;
                out.accepted_at = accepted.terms_accepted_at;
                BOOST_LOG_SEV(provider_terms_handler_lg(), ores::logging::info)
                    << actor << " accepted the terms of provider '" << code << "'";
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(provider_terms_handler_lg(), ores::logging::error)
                << "Accepting the terms of '" << code << "' failed: " << e.what();
            out.success = false;
            out.message = e.what();
        }
        return out;
    }

    // The write claims the version the handler read, so a change that lands between the read
    // and the write is refused rather than overwritten.
    static ores::utility::domain::precondition claim_of(const domain::provider& read) {
        return {ores::utility::domain::precondition_kind::must_match_version,
                static_cast<std::uint32_t>(read.version)};
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
