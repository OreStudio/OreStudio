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
#ifndef ORES_INBOX_CORE_MESSAGING_APPROVAL_OPERATIONS_HANDLER_HPP
#define ORES_INBOX_CORE_MESSAGING_APPROVAL_OPERATIONS_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.api/messaging/approval_operations_protocol.hpp"
#include "ores.inbox.core/presentation/approval_request_history_field_mapper.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.inbox.core/service/approval_request_service.hpp"
#include "ores.inbox.core/service/notification_center.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <functional>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::inbox::messaging {

namespace {

inline auto& approval_operations_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.inbox.messaging.approval_operations_handler");
    return instance;
}

inline ores::utility::domain::result
approval_result(ores::utility::domain::outcome o, std::string code, std::string message) {
    return ores::utility::domain::result{
        .outcome = o, .code = std::move(code), .message = std::move(message), .fields = {}};
}

inline ores::utility::domain::outcome outcome_named(const std::string& name) {
    using ores::utility::domain::outcome;
    if (name == "ok")
        return outcome::ok;
    if (name == "missing")
        return outcome::missing;
    if (name == "conflict")
        return outcome::conflict;
    if (name == "invalid")
        return outcome::invalid;
    return outcome::failed;
}

/**
 * @brief The result a decision answers with: the outcome the database named,
 * with no code when it succeeded, as every other success answers.
 */
inline ores::utility::domain::result decision_reply(const service::decision_result& r) {
    const auto o = outcome_named(r.outcome);
    return approval_result(o, o == ores::utility::domain::outcome::ok ? "" : r.outcome, r.message);
}

/**
 * @brief The words for a rule the decisions table refused, if it was one.
 *
 * The database states a broken lifecycle rule as an error. These are the rules
 * a person can break by acting, so their refusal is a conflict in the person's
 * words; any other error is a failure, and its text stays in the log.
 */
inline std::optional<ores::utility::domain::result> rule_refusal(const std::exception& e) {
    using ores::utility::domain::outcome;
    const std::string_view what = e.what();
    if (what.find("cannot decide request") != std::string_view::npos)
        return approval_result(outcome::conflict,
                               "four_eyes",
                               "You asked for this request, so someone else decides it.");
    if (what.find("one_approval_per_person") != std::string_view::npos)
        return approval_result(
            outcome::conflict, "already_approved", "You have already approved this request.");
    if (what.find("Only the person who asked can withdraw") != std::string_view::npos)
        return approval_result(
            outcome::denied, "not_the_asker", "Only the person who asked can withdraw a request.");
    return std::nullopt;
}

} // namespace

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief Answers the approval request lifecycle operations.
 *
 * Every operation acts as the signed-in person. The handler resolves who that
 * is, checks what they may decide from their token, and leaves the lifecycle
 * rules to the approval lifecycle and the database.
 */
class approval_operations_handler {
public:
    approval_operations_handler(ores::nats::service::client& nats,
                                ores::database::context ctx,
                                std::optional<ores::security::jwt::jwt_authenticator> verifier,
                                std::chrono::seconds answered_window,
                                std::chrono::seconds reminder_window)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier))
        , answered_window_(answered_window)
        , reminder_window_(reminder_window) {}

    void raise(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        auto req = decode<raise_approval_request_request>(msg);
        if (!req) {
            reply(nats_,
                  msg,
                  raise_approval_request_response{
                      .result = approval_result(
                          outcome::invalid, "bad_request", "The request could not be read.")});
            return;
        }
        try {
            service::approval_lifecycle lifecycle(*ctx);
            const auto kind = lifecycle.kind(req->kind_code);
            if (!kind) {
                reply(nats_,
                      msg,
                      raise_approval_request_response{
                          .result = approval_result(outcome::invalid,
                                                    "unknown_kind",
                                                    "No such kind of request: " + req->kind_code)});
                return;
            }
            if (req->reason.empty()) {
                reply(nats_,
                      msg,
                      raise_approval_request_response{
                          .result = approval_result(
                              outcome::invalid, "reason_required", "Say why you ask.")});
                return;
            }
            const auto me = lifecycle.actor_account_id();
            if (!me) {
                reply(nats_,
                      msg,
                      raise_approval_request_response{
                          .result = approval_result(outcome::denied,
                                                    "no_account",
                                                    "The signed-in account was not found.")});
                return;
            }
            for (const auto& code : req->part_codes) {
                if (!lifecycle.part(code)) {
                    reply(nats_,
                          msg,
                          raise_approval_request_response{
                              .result = approval_result(outcome::invalid,
                                                        "unknown_part",
                                                        "No such approval part: " + code)});
                    return;
                }
            }
            const auto raised = lifecycle.raise(*kind, req->reason, *me, req->part_codes);
            tell_open_parts(*ctx, lifecycle, *kind, raised);
            reply(nats_,
                  msg,
                  raise_approval_request_response{.result = approval_result(outcome::ok, "", ""),
                                                  .request = raised});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(approval_operations_handler_lg(), error)
                << "Error raising a request: " << e.what();
            reply(nats_,
                  msg,
                  raise_approval_request_response{
                      .result = approval_result(outcome::failed, "raise_failed", e.what())});
        }
    }

    void withdraw(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        auto req = decode<withdraw_approval_request_request>(msg);
        if (!req) {
            reply(nats_,
                  msg,
                  withdraw_approval_request_response{
                      .result = approval_result(
                          outcome::invalid, "bad_request", "The request could not be read.")});
            return;
        }
        try {
            service::approval_lifecycle lifecycle(*ctx);
            const auto me = lifecycle.actor_account_id();
            if (!me) {
                reply(nats_,
                      msg,
                      withdraw_approval_request_response{
                          .result = approval_result(outcome::denied,
                                                    "no_account",
                                                    "The signed-in account was not found.")});
                return;
            }
            const auto current = lifecycle.request(req->request_id);
            if (!current) {
                reply(nats_,
                      msg,
                      withdraw_approval_request_response{
                          .result =
                              approval_result(outcome::missing, "missing", "No such request.")});
                return;
            }
            // The decisions table refuses a withdrawal by anyone else too; this
            // answers the person plainly before the database has to.
            if (current->requested_by != *me) {
                reply(nats_,
                      msg,
                      withdraw_approval_request_response{
                          .result =
                              approval_result(outcome::denied,
                                              "not_the_asker",
                                              "Only the person who asked can withdraw a request."),
                          .request = *current});
                return;
            }
            const auto r =
                lifecycle.decide(req->request_id, req->version, "withdraw", *me, req->comment);
            reply(nats_,
                  msg,
                  withdraw_approval_request_response{
                      .result = decision_reply(r),
                      .request = lifecycle.request(req->request_id).value_or(*current)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(approval_operations_handler_lg(), error)
                << "Error withdrawing request " << req->request_id << ": " << e.what();
            reply(nats_,
                  msg,
                  withdraw_approval_request_response{
                      .result = rule_refusal(e).value_or(
                          approval_result(outcome::failed,
                                          "withdraw_failed",
                                          "The withdrawal could not be recorded."))});
        }
    }

    void decide(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        auto req = decode<decide_approval_request_request>(msg);
        if (!req) {
            reply(nats_,
                  msg,
                  decide_approval_request_response{
                      .result = approval_result(
                          outcome::invalid, "bad_request", "The request could not be read.")});
            return;
        }
        if (req->decision_code == "withdraw") {
            reply(
                nats_,
                msg,
                decide_approval_request_response{
                    .result = approval_result(outcome::invalid,
                                              "use_withdraw",
                                              "A request is withdrawn by the person who asked.")});
            return;
        }
        try {
            service::approval_lifecycle lifecycle(*ctx);
            const auto current = lifecycle.request(req->request_id);
            if (!current) {
                reply(nats_,
                      msg,
                      decide_approval_request_response{
                          .result =
                              approval_result(outcome::missing, "missing", "No such request.")});
                return;
            }
            const auto kind = lifecycle.kind(current->kind_code);
            if (!kind) {
                reply(nats_,
                      msg,
                      decide_approval_request_response{
                          .result = approval_result(outcome::denied,
                                                    "not_a_decider",
                                                    "You may not decide this kind of request.")});
                return;
            }
            const auto parts = lifecycle.parts_of(req->request_id);
            const bool answers_for_part =
                !parts.empty() && (req->decision_code == "approve" || req->decision_code == "refuse");
            if (answers_for_part) {
                const auto named = std::ranges::find_if(
                    parts, [&](const auto& p) { return p.code == req->part_code; });
                if (named == parts.end()) {
                    reply(nats_,
                          msg,
                          decide_approval_request_response{
                              .result = approval_result(outcome::invalid,
                                                        "part_required",
                                                        "Name one of the parts this request needs.")});
                    return;
                }
                if (!has_permission(*ctx, named->decide_permission_code)) {
                    reply(nats_,
                          msg,
                          decide_approval_request_response{
                              .result = approval_result(outcome::denied,
                                                        "not_a_decider",
                                                        "You may not decide for this part.")});
                    return;
                }
            } else {
                const bool may_decide =
                    parts.empty() ? has_permission(*ctx, kind->decide_permission_code)
                                  : std::ranges::any_of(parts, [&](const auto& p) {
                                        return has_permission(*ctx, p.decide_permission_code);
                                    });
                if (!may_decide) {
                    reply(nats_,
                          msg,
                          decide_approval_request_response{
                              .result = approval_result(outcome::denied,
                                                        "not_a_decider",
                                                        "You may not decide this kind of request.")});
                    return;
                }
            }
            const auto me = lifecycle.actor_account_id();
            if (!me) {
                reply(nats_,
                      msg,
                      decide_approval_request_response{
                          .result = approval_result(outcome::denied,
                                                    "no_account",
                                                    "The signed-in account was not found.")});
                return;
            }
            const auto r = lifecycle.decide(req->request_id,
                                            req->version,
                                            req->decision_code,
                                            *me,
                                            req->comment,
                                            answers_for_part ? req->part_code : std::string{});
            const auto after = lifecycle.request(req->request_id).value_or(*current);
            if (after.state_code != current->state_code)
                tell_asker(*ctx, *kind, after, req->comment);
            else if (answers_for_part && r.outcome == "ok" && req->decision_code == "approve")
                tell_open_parts(*ctx, lifecycle, *kind, after);
            reply(nats_,
                  msg,
                  decide_approval_request_response{.result = decision_reply(r), .request = after});
        } catch (const std::exception& e) {
            const auto refusal = rule_refusal(e);
            BOOST_LOG_SEV(approval_operations_handler_lg(), refusal ? warn : error)
                << "Decision on request " << req->request_id << " not recorded: " << e.what();
            reply(
                nats_,
                msg,
                decide_approval_request_response{
                    .result = refusal.value_or(approval_result(
                        outcome::failed, "decide_failed", "The decision could not be recorded."))});
        }
    }

    void queue(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        auto req = decode<list_approval_queue_request>(msg);
        if (!req) {
            reply(nats_,
                  msg,
                  list_approval_queue_response{
                      .result = approval_result(
                          outcome::invalid, "bad_request", "The request could not be read.")});
            return;
        }
        try {
            service::approval_lifecycle lifecycle(*ctx);
            const auto me = lifecycle.actor_account_id();
            if (!me) {
                reply(nats_,
                      msg,
                      list_approval_queue_response{
                          .result = approval_result(outcome::denied,
                                                    "no_account",
                                                    "The signed-in account was not found.")});
                return;
            }
            std::vector<std::string> decidable;
            for (const auto& k : lifecycle.kinds())
                if (has_permission(*ctx, k.decide_permission_code))
                    decidable.push_back(k.code);
            auto page = lifecycle.queue(decidable, *me, req->offset, req->limit);
            auto answered = lifecycle.recently_answered(decidable, *me, answered_window_);
            reply(nats_,
                  msg,
                  list_approval_queue_response{.result = approval_result(outcome::ok, "", ""),
                                               .requests = std::move(page.requests),
                                               .total = page.total,
                                               .answered = std::move(answered)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(approval_operations_handler_lg(), error)
                << "Error reading the approval queue: " << e.what();
            reply(nats_,
                  msg,
                  list_approval_queue_response{
                      .result = approval_result(outcome::failed, "queue_failed", e.what())});
        }
    }

    /**
     * @brief Reads the one request an identifier names, if the caller may open
     * it.
     *
     * A notice carries the request it is about, and a notice is usually read
     * after the request stopped waiting, so the queue cannot answer this. What
     * the caller may open is the person who asked, or whoever may decide a
     * request of that kind. A request the caller may not see is answered as
     * absent rather than refused, so the reply says nothing about a request
     * the caller has no business knowing about.
     */
    void get_request(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        auto req = decode<get_approval_request>(msg);
        if (!req) {
            reply(nats_,
                  msg,
                  get_approval_response{
                      .result = approval_result(
                          outcome::invalid, "bad_request", "The request could not be read.")});
            return;
        }
        try {
            service::approval_lifecycle lifecycle(*ctx);
            const auto found = lifecycle.request(req->request_id);
            if (!found || !may_open(*ctx, lifecycle, *found)) {
                reply(nats_,
                      msg,
                      get_approval_response{
                          .result = approval_result(outcome::missing, "not_found", "")});
                return;
            }
            reply(nats_,
                  msg,
                  get_approval_response{.result = approval_result(outcome::ok, "", ""),
                                        .request = *found});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(approval_operations_handler_lg(), error)
                << "Error reading approval request " << req->request_id << ": " << e.what();
            reply(nats_,
                  msg,
                  get_approval_response{
                      .result = approval_result(outcome::failed, "read_failed", e.what())});
        }
    }

    /**
     * @brief Reads every version of one request, newest first.
     *
     * The same entitlement as opening the request, because the versions are the
     * request with a clock on it. The generic history read gates on the
     * administrator's permission and so cannot answer the person who raised the
     * request with their own request's versions, which is the whole reason this
     * read exists.
     */
    void get_history(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        auto req = decode<get_approval_history_request>(msg);
        if (!req) {
            reply(nats_,
                  msg,
                  get_approval_history_response{
                      .result = approval_result(
                          outcome::invalid, "bad_request", "The request could not be read.")});
            return;
        }
        try {
            service::approval_lifecycle lifecycle(*ctx);
            const auto found = lifecycle.request(req->request_id);
            if (!found || !may_open(*ctx, lifecycle, *found)) {
                reply(nats_,
                      msg,
                      get_approval_history_response{
                          .result = approval_result(outcome::missing, "not_found", "")});
                return;
            }

            service::approval_request_service requests(*ctx);
            const auto versions = requests.get_request_history(req->request_id);
            std::vector<approval_request_version> answer;
            answer.reserve(versions.size());
            for (const auto& v : versions) {
                approval_request_version rendered;
                rendered.version = v.version;
                rendered.modified_by = v.modified_by;
                rendered.performed_by = v.performed_by;
                rendered.recorded_at = v.recorded_at;
                rendered.change_reason_code = v.change_reason_code;
                rendered.change_commentary = v.change_commentary;
                for (const auto& field : presentation::render_approval_request_fields(v))
                    rendered.fields.push_back(
                        approval_request_field{.name = field.name, .value = field.value});
                answer.push_back(std::move(rendered));
            }
            std::ranges::sort(answer, std::greater{}, &approval_request_version::version);
            reply(nats_,
                  msg,
                  get_approval_history_response{.result = approval_result(outcome::ok, "", ""),
                                                .versions = std::move(answer)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(approval_operations_handler_lg(), error)
                << "Error reading the history of approval request " << req->request_id << ": "
                << e.what();
            reply(nats_,
                  msg,
                  get_approval_history_response{
                      .result = approval_result(outcome::failed, "read_failed", e.what())});
        }
    }

    /**
     * @brief Whether the caller may open a request and read its trail.
     *
     * The person who raised it, whoever may decide its kind, and whoever may
     * read the tenant's requests. Answered as absent rather than refused, so the
     * reply says nothing about a request the caller has no business knowing.
     */
    bool may_open(const ores::database::context& ctx,
                  service::approval_lifecycle& lifecycle,
                  const domain::approval_request& request) {
        const auto me = lifecycle.actor_account_id();
        const auto kind = lifecycle.kind(request.kind_code);
        return (me && request.requested_by == *me) ||
               (kind && has_permission(ctx, kind->decide_permission_code)) ||
               has_permission(ctx, "inbox::approval_requests:read");
    }

    void mine(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        auto ctx = context_for(msg);
        if (!ctx)
            return;
        auto req = decode<list_my_approval_requests_request>(msg);
        if (!req) {
            reply(nats_,
                  msg,
                  list_my_approval_requests_response{
                      .result = approval_result(
                          outcome::invalid, "bad_request", "The request could not be read.")});
            return;
        }
        try {
            service::approval_lifecycle lifecycle(*ctx);
            const auto me = lifecycle.actor_account_id();
            if (!me) {
                reply(nats_,
                      msg,
                      list_my_approval_requests_response{
                          .result = approval_result(outcome::denied,
                                                    "no_account",
                                                    "The signed-in account was not found.")});
                return;
            }
            auto page = lifecycle.raised_by(*me, req->offset, req->limit);
            reply(nats_,
                  msg,
                  list_my_approval_requests_response{.result = approval_result(outcome::ok, "", ""),
                                                     .requests = std::move(page.requests),
                                                     .total = page.total});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(approval_operations_handler_lg(), error)
                << "Error reading one's own requests: " << e.what();
            reply(nats_,
                  msg,
                  list_my_approval_requests_response{
                      .result = approval_result(outcome::failed, "mine_failed", e.what())});
        }
    }

    /**
     * @brief Closes every open request past its kind's deadline, and tells
     * each person who asked.
     *
     * The scheduler fires this operation, so the caller is the service rather
     * than a person and the sweep reaches every tenant. A firing is a plain
     * publish that carries no token, so the service's own context is what
     * holds the work: there is no session to read one from.
     */
    /**
     * @brief Warns the deciders of every request close to its deadline.
     *
     * The scheduler fires this, so it acts as the service rather than as a
     * person and reaches requests no tenant-scoped caller could read. Each
     * request is warned about once: the notice already raised names it, so a
     * repeated call finds nothing new.
     */
    void remind_expiring(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        try {
            service::approval_lifecycle lifecycle(ctx_);
            const auto expiring = lifecycle.remind_expiring(reminder_window_);
            std::vector<std::string> ids;
            ids.reserve(expiring.size());
            for (const auto& e : expiring)
                ids.push_back(e.request_id);
            reply(nats_,
                  msg,
                  remind_expiring_approvals_response{.result = approval_result(outcome::ok, "", ""),
                                                     .reminded = std::move(ids)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(approval_operations_handler_lg(), error)
                << "Error warning the deciders of requests close to their deadline: " << e.what();
            reply(nats_,
                  msg,
                  remind_expiring_approvals_response{
                      .result = approval_result(outcome::failed, "remind_failed", e.what())});
        }
    }

    void expire_overdue(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        try {
            service::approval_lifecycle lifecycle(ctx_);
            const auto expired = lifecycle.expire_overdue();
            std::vector<std::string> ids;
            ids.reserve(expired.size());
            for (const auto& e : expired)
                ids.push_back(e.request_id);
            reply(nats_,
                  msg,
                  expire_overdue_approvals_response{.result = approval_result(outcome::ok, "", ""),
                                                    .expired = std::move(ids)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(approval_operations_handler_lg(), error)
                << "Error closing the requests nobody answered: " << e.what();
            reply(nats_,
                  msg,
                  expire_overdue_approvals_response{
                      .result = approval_result(outcome::failed, "expire_failed", e.what())});
        }
    }

private:
    /**
     * @brief Tells the deciders whose turn it is.
     *
     * A request of a kind with one decider permission tells its holders. A
     * request that names parts tells the holders of the parts that are open:
     * those of the earliest answer order that has not yet approved.
     */
    void tell_open_parts(const ores::database::context& ctx,
                         service::approval_lifecycle& lifecycle,
                         const domain::approval_kind& kind,
                         const domain::approval_request& raised) {
        const auto parts = lifecycle.parts_of(boost::uuids::to_string(raised.id));
        if (parts.empty()) {
            tell_deciders(ctx, kind, raised, kind.decide_permission_code);
            return;
        }
        for (const auto& open : lifecycle.open_parts_of(boost::uuids::to_string(raised.id)))
            tell_deciders(ctx, kind, raised, open.decide_permission_code);
    }

    /**
     * @brief Tells the people who may decide a request that it waits.
     *
     * Telling is never the operation: a failure here is logged, and the
     * request stands.
     */
    void tell_deciders(const ores::database::context& ctx,
                       const domain::approval_kind& kind,
                       const domain::approval_request& raised,
                       const std::string& permission_code) {
        try {
            service::notification_center center(ctx);
            auto deciders = center.holders_of(permission_code);
            std::erase(deciders, boost::uuids::to_string(raised.requested_by));
            if (deciders.empty())
                return;
            raise_notification_request n{.kind_code = "inbox.approval_waiting",
                                         .link_route = "requests",
                                         .link_id = boost::uuids::to_string(raised.id),
                                         .arguments = {{.name = "kind", .value = kind.name},
                                                       {.name = "requester", .value = ctx.actor()},
                                                       {.name = "reason", .value = raised.reason}},
                                         .account_ids = {},
                                         .audience_permission_code = permission_code};
            center.raise(n, deciders, raised.requested_by);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(approval_operations_handler_lg(), warn)
                << "Request " << boost::uuids::to_string(raised.id)
                << " raised, but its deciders were not told: " << e.what();
        }
    }

    /**
     * @brief Tells the person who asked that their request moved.
     */
    void tell_asker(const ores::database::context& ctx,
                    const domain::approval_kind& kind,
                    const domain::approval_request& after,
                    const std::string& comment) {
        try {
            service::notification_center center(ctx);
            const auto decider = center.actor_account_id();
            if (!decider)
                return;
            raise_notification_request n{
                .kind_code = "inbox.approval_decided",
                .link_route = "requests",
                .link_id = boost::uuids::to_string(after.id),
                .arguments = {{.name = "kind", .value = kind.name},
                              {.name = "state", .value = after.state_code},
                              {.name = "decider", .value = ctx.actor()},
                              {.name = "comment", .value = comment}},
                .account_ids = {boost::uuids::to_string(after.requested_by)},
                .audience_permission_code = ""};
            center.raise(n, n.account_ids, *decider);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(approval_operations_handler_lg(), warn)
                << "Request " << boost::uuids::to_string(after.id)
                << " decided, but the person who asked was not told: " << e.what();
        }
    }

    std::optional<ores::database::context> context_for(const ores::nats::message& msg) {
        BOOST_LOG_SEV(approval_operations_handler_lg(), debug) << "Handling " << msg.subject;
        auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx) {
            error_reply(nats_, msg, ctx.error());
            return std::nullopt;
        }
        return *ctx;
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    std::chrono::seconds answered_window_;
    std::chrono::seconds reminder_window_;
};

}

#endif
