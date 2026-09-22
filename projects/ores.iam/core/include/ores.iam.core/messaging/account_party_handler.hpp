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
#ifndef ORES_IAM_MESSAGING_ACCOUNT_PARTY_HANDLER_HPP
#define ORES_IAM_MESSAGING_ACCOUNT_PARTY_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.iam.api/messaging/account_party_protocol.hpp"
#include "ores.iam.core/service/account_party_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <boost/uuid/string_generator.hpp>
#include <cstddef>
#include <stdexcept>

namespace ores::iam::messaging {

namespace {

inline auto& account_party_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.iam.messaging.account_party_handler");
    return instance;
}

/**
 * @brief DELIBERATE, NARROW REPLACEMENT for
 * ores::service::messaging::stamp() — read this before touching
 * either function.
 *
 * @warning Do NOT "simplify" this by switching back to the generic
 * stamp(ap, ctx). That generic helper treats any field named
 * party_id as a security boundary and unconditionally overwrites it
 * with the caller's own current party from the JWT context — correct
 * for every other domain type, where party_id means "the party this
 * row belongs to." account_party is the one exception in the
 * codebase: its party_id means "the party being targeted by this
 * association," an arbitrary value the *client* is supposed to
 * supply (e.g. associating an admin with some other party). Because
 * the field happens to share the name, the generic stamp() matched
 * on it by reflection and silently clobbered every requested
 * association with the caller's own party instead — this is exactly
 * the bug that caused provision_tenant's Phase 3 to associate the
 * tenant admin only with their own System Party, no matter which
 * Operational parties were actually requested (see the account_party
 * capture doc in doc/agile/product_backlog/inbox/ for the full story).
 *
 * If you ever add another field to account_party that collides in
 * name with a field stamp() treats as a security boundary (tenant_id,
 * party_id, and anything added to stamp() in future), you must extend
 * *this* function to handle it explicitly rather than reaching for
 * the generic helper. Any other handler considering a client-supplied
 * "target party/tenant/etc. that is not this row's own scope" field
 * should follow the same pattern: a small, explicit, per-type stamp
 * function, not the generic reflection-based one.
 */
inline void stamp_account_party(domain::account_party& ap, const ores::database::context& ctx) {
    ap.tenant_id = ctx.tenant_id().to_string();
    const auto& actor = ctx.actor();
    const auto& svc = ctx.service_account();
    if (!actor.empty())
        ap.modified_by = actor;
    else if (!svc.empty())
        ap.modified_by = svc;
    if (!svc.empty())
        ap.performed_by = svc;
    if (ap.change_reason_code.empty())
        ap.change_reason_code = std::string(ores::service::messaging::change_reasons::new_record);
}

} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using namespace ores::logging;

/**
 * @brief NATS message handler for the account-party associations.
 *
 * The associations are a resource like any other, so the handler serves the
 * canonical verbs and decides nothing: it proves the request, checks the
 * permission a write needs, decodes the canonical request, calls the service
 * and replies with the response it filled. The outcome a caller reads is the
 * service's answer.
 *
 * @note Two things are specific to this resource and must stay that way. Its
 * party_id names the party being associated, not the caller's own party, so
 * the write is stamped by stamp_account_party rather than the generic stamp
 * (read that function before touching this one). And linking an account to a
 * party is a provisioning workflow step, so the batch write keeps the
 * workflow-command path that reports a step outcome back to the orchestrator.
 */
class account_party_handler {
public:
    account_party_handler(ores::nats::service::client& nats,
                          ores::database::context ctx,
                          ores::security::jwt::jwt_authenticator signer)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , signer_(std::move(signer)) {}

    void list_account_parties(ores::nats::message msg) {
        auto ctx_expected = begin_request(msg);
        if (!ctx_expected)
            return;
        auto req = decode<list_account_parties_request>(msg);
        if (!req) {
            bad_request(msg);
            return;
        }
        list_account_parties_response response;
        if (refuse_stated_order(msg, req->order, response.result))
            return;
        if (refuse_stated_filter(msg, req->filter.has_value(), response.result))
            return;
        try {
            service::account_party_service svc(*ctx_expected);
            response.account_parties = svc.list_account_parties(req->offset, req->limit);
            response.total = svc.get_total_account_party_count();
            complete(msg);
        } catch (const std::exception& e) {
            failed(msg, e, response.result);
        }
        reply(nats_, msg, response);
    }

    void list_by_account_id_account_parties(ores::nats::message msg) {
        auto ctx_expected = begin_request(msg);
        if (!ctx_expected)
            return;
        auto req = decode<list_by_account_id_account_parties_request>(msg);
        if (!req) {
            bad_request(msg);
            return;
        }
        list_by_account_id_account_parties_response response;
        if (refuse_stated_order(msg, req->order, response.result))
            return;
        if (refuse_stated_filter(msg, req->filter.has_value(), response.result))
            return;
        if (refuse_subtree_scope(msg, req->scope, response.result))
            return;
        try {
            service::account_party_service svc(*ctx_expected);
            response.account_parties =
                svc.list_account_parties_by_account(req->account_id, req->offset, req->limit);
            response.total = svc.get_total_account_party_count_by_account(req->account_id);
            complete(msg);
        } catch (const std::exception& e) {
            failed(msg, e, response.result);
        }
        reply(nats_, msg, response);
    }

    void get_account_party(ores::nats::message msg) {
        auto ctx_expected = begin_request(msg);
        if (!ctx_expected)
            return;
        auto req = decode<get_account_party_request>(msg);
        if (!req) {
            bad_request(msg);
            return;
        }
        get_account_party_response response;
        try {
            service::account_party_service svc(*ctx_expected);
            auto found = svc.find_account_party(req->key.account_id, req->key.party_id);
            if (!found) {
                missing(response.result);
            } else {
                response.account_party = std::move(*found);
                complete(msg);
            }
        } catch (const std::exception& e) {
            failed(msg, e, response.result);
        }
        reply(nats_, msg, response);
    }

    void get_many_account_parties(ores::nats::message msg) {
        auto ctx_expected = begin_request(msg);
        if (!ctx_expected)
            return;
        auto req = decode<get_many_account_parties_request>(msg);
        if (!req) {
            bad_request(msg);
            return;
        }
        get_many_account_parties_response response;
        try {
            service::account_party_service svc(*ctx_expected);
            // One entry per requested key, in the order asked for, so the reply
            // is positional and a caller reads absence from an empty entry.
            response.entries.reserve(req->keys.size());
            for (const auto& key : req->keys) {
                account_party_lookup entry;
                entry.key = key;
                auto found = svc.find_account_party(key.account_id, key.party_id);
                if (found)
                    entry.account_party = std::move(*found);
                response.entries.push_back(std::move(entry));
            }
            complete(msg);
        } catch (const std::exception& e) {
            failed(msg, e, response.result);
        }
        reply(nats_, msg, response);
    }

    void put_account_party(ores::nats::message msg) {
        auto ctx_expected = begin_request(msg);
        if (!ctx_expected)
            return;
        if (!may_update(msg, *ctx_expected))
            return;
        auto req = decode<put_account_party_request>(msg);
        if (!req) {
            bad_request(msg);
            return;
        }
        put_account_party_response response;
        try {
            service::account_party_service svc(*ctx_expected);
            auto link = to_domain(req->change.write);
            // The association did not exist before, so a precondition that the
            // caller stated differently is refused rather than ignored.
            if (req->change.precondition.kind == ores::utility::domain::precondition_kind::any ||
                req->change.precondition.kind ==
                    ores::utility::domain::precondition_kind::must_not_exist) {
                auto current = svc.find_account_party(link.account_id, link.party_id);
                if (current && req->change.precondition.kind ==
                                   ores::utility::domain::precondition_kind::must_not_exist) {
                    response.result.outcome = ores::utility::domain::outcome::conflict;
                    response.result.code = "already_exists";
                } else {
                    stamp_account_party(link, *ctx_expected);
                    svc.save_account_party(link);
                    auto written = svc.find_account_party(link.account_id, link.party_id);
                    if (written)
                        response.account_party = std::move(*written);
                    complete(msg);
                }
            } else {
                response.result.outcome = ores::utility::domain::outcome::invalid;
                response.result.code = "precondition_not_supported";
                response.result.message = "An association keeps no version to match.";
            }
        } catch (const std::exception& e) {
            failed(msg, e, response.result);
        }
        reply(nats_, msg, response);
    }

    void put_many_account_parties(ores::nats::message msg) {
        // Workflow step command: bypass JWT auth; use X-Tenant-Id for context.
        if (ores::service::messaging::is_workflow_command(msg)) {
            handle_workflow_step(msg);
            return;
        }
        auto ctx_expected = begin_request(msg);
        if (!ctx_expected)
            return;
        if (!may_update(msg, *ctx_expected))
            return;
        auto req = decode<put_many_account_parties_request>(msg);
        if (!req) {
            bad_request(msg);
            return;
        }
        put_many_account_parties_response response;
        try {
            service::account_party_service svc(*ctx_expected);
            std::vector<domain::account_party> batch;
            batch.reserve(req->changes.size());
            for (const auto& change : req->changes) {
                auto link = to_domain(change.write);
                stamp_account_party(link, *ctx_expected);
                batch.push_back(std::move(link));
            }
            for (const auto& link : batch)
                svc.save_account_party(link);
            response.account_parties.reserve(batch.size());
            for (const auto& link : batch) {
                auto written = svc.find_account_party(link.account_id, link.party_id);
                response.account_parties.push_back(written ? std::move(*written) : link);
            }
            complete(msg);
        } catch (const std::exception& e) {
            failed(msg, e, response.result);
        }
        reply(nats_, msg, response);
    }

    void delete_account_party(ores::nats::message msg) {
        auto ctx_expected = begin_request(msg);
        if (!ctx_expected)
            return;
        if (!may_update(msg, *ctx_expected))
            return;
        auto req = decode<delete_account_party_request>(msg);
        if (!req) {
            bad_request(msg);
            return;
        }
        delete_account_party_response response;
        try {
            service::account_party_service svc(*ctx_expected);
            svc.remove_account_party(req->removal.key.account_id, req->removal.key.party_id);
            complete(msg);
        } catch (const std::exception& e) {
            failed(msg, e, response.result);
        }
        reply(nats_, msg, response);
    }

    void delete_many_account_parties(ores::nats::message msg) {
        auto ctx_expected = begin_request(msg);
        if (!ctx_expected)
            return;
        if (!may_update(msg, *ctx_expected))
            return;
        auto req = decode<delete_many_account_parties_request>(msg);
        if (!req) {
            bad_request(msg);
            return;
        }
        delete_many_account_parties_response response;
        try {
            service::account_party_service svc(*ctx_expected);
            for (const auto& removal : req->removals)
                svc.remove_account_party(removal.key.account_id, removal.key.party_id);
            complete(msg);
        } catch (const std::exception& e) {
            failed(msg, e, response.result);
        }
        reply(nats_, msg, response);
    }

private:
    /**
     * @brief Proves the request and logs the entry.
     *
     * @return the request context, or nothing after replying with the reason
     * the request could not be served.
     */
    std::optional<ores::database::context> begin_request(const ores::nats::message& msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(account_party_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return std::nullopt;
        }
        return *ctx_expected;
    }

    /** @return true when the caller holds the permission the write needs. */
    bool may_update(const ores::nats::message& msg, const ores::database::context& ctx) {
        if (has_permission(ctx, "iam::accounts:update"))
            return true;
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return false;
    }

    void bad_request(const ores::nats::message& msg) {
        BOOST_LOG_SEV(account_party_handler_lg(), warn) << "Failed to decode: " << msg.subject;
        error_reply(nats_, msg, ores::service::error_code::bad_request);
    }

    void complete(const ores::nats::message& msg) {
        BOOST_LOG_SEV(account_party_handler_lg(), debug) << "Completed " << msg.subject;
    }

    void failed(const ores::nats::message& msg,
                const std::exception& e,
                ores::utility::domain::result& result) {
        BOOST_LOG_SEV(account_party_handler_lg(), error) << msg.subject << " failed: " << e.what();
        result.outcome = ores::utility::domain::outcome::failed;
        result.code = "internal_error";
        result.message = e.what();
    }

    void missing(ores::utility::domain::result& result) {
        result.outcome = ores::utility::domain::outcome::missing;
        result.code = "not_found";
    }

    /**
     * @brief Refuses a stated order, which the store cannot answer.
     *
     * A page of an unordered set repeats or skips rows, so an order the store
     * cannot serve is refused rather than silently ignored.
     */
    bool refuse_stated_order(const ores::nats::message& msg,
                             const ores::utility::domain::order& order,
                             ores::utility::domain::result& result) {
        if (order.field.empty() && !order.descending)
            return false;
        BOOST_LOG_SEV(account_party_handler_lg(), warn)
            << msg.subject << " rejected: order " << order.field << " is not served";
        result.outcome = ores::utility::domain::outcome::invalid;
        result.code = "order_not_supported";
        result.message = "This store pages in key order and cannot order by a stated field.";
        return true;
    }

    bool refuse_stated_filter(const ores::nats::message& msg,
                              bool filter_stated,
                              ores::utility::domain::result& result) {
        if (!filter_stated)
            return false;
        BOOST_LOG_SEV(account_party_handler_lg(), warn)
            << msg.subject << " rejected: filtering is not served";
        result.outcome = ores::utility::domain::outcome::invalid;
        result.code = "filter_not_supported";
        result.message = "Filtering is not served for this resource yet.";
        return true;
    }

    bool refuse_subtree_scope(const ores::nats::message& msg,
                              ores::utility::domain::scope scope,
                              ores::utility::domain::result& result) {
        if (scope != ores::utility::domain::scope::subtree)
            return false;
        BOOST_LOG_SEV(account_party_handler_lg(), warn)
            << msg.subject << " rejected: the resource has no subtree";
        result.outcome = ores::utility::domain::outcome::invalid;
        result.code = "scope_not_supported";
        result.message = "This resource reads its direct members; it has no subtree.";
        return true;
    }

    /**
     * @brief Builds the domain association a write record states.
     *
     * The record carries the user-owned fields alone; the tenant, the
     * provenance and the validity window are stamped after this.
     */
    static domain::account_party to_domain(const account_party_write& write) {
        domain::account_party link;
        link.account_id = write.account_id;
        link.party_id = write.party_id;
        return link;
    }

    /**
     * @brief Serves a provisioning workflow's link step.
     *
     * The step is idempotent: a replayed step reports the outcome it already
     * published rather than linking the pair a second time.
     */
    void handle_workflow_step(const ores::nats::message& msg) {
        using ores::service::messaging::extract_workflow_header;
        using ores::service::messaging::publish_step_completion;
        using ores::service::messaging::check_step_idempotency;
        using ores::service::messaging::workflow_step_id_header;
        using ores::service::messaging::workflow_instance_id_header;
        using ores::service::messaging::workflow_tenant_id_header;

        const auto step_id = extract_workflow_header(msg, workflow_step_id_header);
        const auto inst_id = extract_workflow_header(msg, workflow_instance_id_header);
        const auto tenant_id = extract_workflow_header(msg, workflow_tenant_id_header);

        if (auto cached = check_step_idempotency(nats_, step_id)) {
            publish_step_completion(nats_,
                                    step_id,
                                    inst_id,
                                    cached->outcome,
                                    cached->result_json,
                                    cached->error_message,
                                    cached->log);
            return;
        }

        auto req = decode<put_many_account_parties_request>(msg);
        if (!req) {
            publish_step_completion(nats_,
                                    step_id,
                                    inst_id,
                                    ores::workflow::messaging::step_outcome::failed,
                                    "",
                                    "Failed to decode put_many_account_parties_request");
            return;
        }
        try {
            using ores::database::service::tenant_context;
            auto wf_ctx = tenant_context::with_tenant(ctx_, tenant_id);
            service::account_party_service svc(wf_ctx);
            for (const auto& change : req->changes) {
                auto link = to_domain(change.write);
                stamp_account_party(link, wf_ctx);
                svc.save_account_party(link);
            }
            BOOST_LOG_SEV(account_party_handler_lg(), debug)
                << "Workflow step completed: " << msg.subject;
            publish_step_completion(nats_,
                                    step_id,
                                    inst_id,
                                    ores::workflow::messaging::step_outcome::completed,
                                    rfl::json::write(put_many_account_parties_response{}),
                                    "");
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(account_party_handler_lg(), error)
                << "Workflow step failed: " << msg.subject << " - " << e.what();
            publish_step_completion(nats_,
                                    step_id,
                                    inst_id,
                                    ores::workflow::messaging::step_outcome::failed,
                                    "",
                                    e.what());
        }
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
};

} // namespace ores::iam::messaging
#endif
