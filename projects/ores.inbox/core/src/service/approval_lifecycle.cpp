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
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.inbox.core/repository/approval_kind_repository.hpp"
#include "ores.inbox.core/repository/approval_part_repository.hpp"
#include "ores.inbox.core/repository/approval_request_part_repository.hpp"
#include "ores.inbox.core/repository/approval_request_repository.hpp"
#include "ores.inbox.core/service/notification_center.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <functional>
#include <stdexcept>

namespace ores::inbox::service {

using namespace ores::logging;

namespace {

constexpr int max_page = 500;

/**
 * @brief How many answered requests the tail carries at most.
 *
 * The window says how far back the tail reaches; this says how wide it is. A
 * busy tenant answers more in a day than anybody reads, and nobody acts on an
 * answered request, so the tail is a glance and not a list.
 */
constexpr int answered_tail_limit = 20;

int clamp_limit(int limit) {
    return std::clamp(limit, 1, max_page);
}

}

approval_lifecycle::approval_lifecycle(ores::database::context ctx)
    : ctx_(std::move(ctx)) {}

std::optional<boost::uuids::uuid> approval_lifecycle::actor_account_id() {
    ores::iam::repository::account_repository accounts;
    const auto found = accounts.read_latest_by_username(ctx_, ctx_.actor());
    if (found.empty())
        return std::nullopt;
    return found.front().id;
}

std::optional<domain::approval_kind> approval_lifecycle::kind(const std::string& code) {
    repository::approval_kind_repository repo;
    const auto system_ctx = ctx_.with_tenant(utility::uuid::tenant_id::system(), ctx_.actor());
    const auto found = repo.read_latest(system_ctx, code);
    if (found.empty())
        return std::nullopt;
    return found.front();
}

std::vector<domain::approval_kind> approval_lifecycle::kinds() {
    repository::approval_kind_repository repo;
    return repo.read_latest(ctx_.with_tenant(utility::uuid::tenant_id::system(), ctx_.actor()));
}

std::optional<domain::approval_request> approval_lifecycle::request(const std::string& id) {
    repository::approval_request_repository repo;
    const auto found = repo.read_latest(ctx_, id);
    if (found.empty())
        return std::nullopt;
    return found.front();
}

domain::approval_request approval_lifecycle::raise(const domain::approval_kind& kind,
                                                   const std::string& reason,
                                                   const boost::uuids::uuid& requested_by,
                                                   const std::vector<std::string>& part_codes) {
    std::vector<std::string> codes;
    for (const auto& code : part_codes) {
        if (code.empty())
            throw std::invalid_argument("A part code cannot be empty.");
        if (std::ranges::find(codes, code) == codes.end())
            codes.push_back(code);
    }

    const auto now = std::chrono::system_clock::now();

    domain::approval_request r;
    r.tenant_id = ctx_.tenant_id();
    r.id = utility::uuid::uuid_v7_generator{}();
    r.kind_code = kind.code;
    r.state_code = "waiting";
    r.requested_by = requested_by;
    r.requested_at = now;
    r.reason = reason;
    if (kind.expires_after_days)
        r.expires_at = now + std::chrono::days(*kind.expires_after_days);
    r.modified_by = ctx_.actor();
    r.change_reason_code = "system.new_record";

    BOOST_LOG_SEV(lg(), info) << "Raising a " << kind.code << " request for "
                              << boost::uuids::to_string(requested_by);
    repository::approval_request_repository repo;
    repo.write(ctx_, r, ores::utility::domain::precondition{});

    if (!codes.empty()) {
        std::vector<domain::approval_request_part> links;
        for (const auto& code : codes) {
            domain::approval_request_part link;
            link.tenant_id = ctx_.tenant_id().to_string();
            link.request_id = r.id;
            link.part_code = code;
            link.modified_by = ctx_.actor();
            link.change_reason_code = "system.new_record";
            links.push_back(std::move(link));
        }
        repository::approval_request_part_repository parts_repo(ctx_);
        try {
            parts_repo.write(links);
        } catch (...) {
            // A request with no part rows is decided by count, so one whose
            // links failed to write must not stay open.
            BOOST_LOG_SEV(lg(), error) << "The parts of request " << boost::uuids::to_string(r.id)
                                       << " were not written; withdrawing it.";
            try {
                const auto current = request(boost::uuids::to_string(r.id));
                decide(boost::uuids::to_string(r.id),
                       current ? current->version : 1,
                       "withdraw",
                       requested_by,
                       "The parts of the request could not be written.");
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error) << "The half-raised request was not withdrawn: " << e.what();
            }
            throw;
        }
    }

    const auto written = request(boost::uuids::to_string(r.id));
    if (!written)
        throw std::runtime_error("The raised request could not be read back.");
    return *written;
}

std::optional<domain::approval_part> approval_lifecycle::part(const std::string& code) {
    // Parts are system tenant lookups, so they are read under the system tenant.
    repository::approval_part_repository repo;
    const auto system_ctx = ctx_.with_tenant(utility::uuid::tenant_id::system(), ctx_.actor());
    const auto found = repo.read_latest(system_ctx, code);
    if (found.empty())
        return std::nullopt;
    return found.front();
}

std::vector<domain::approval_part> approval_lifecycle::parts_of(const std::string& request_id) {
    const auto rows = ores::database::repository::execute_parameterized_multi_column_query(
        ctx_,
        "select rp.part_code "
        "from ores_inbox_approval_request_parts_tbl rp "
        "join ores_inbox_approval_parts_tbl p "
        "  on p.tenant_id = ores_utility_system_tenant_id_fn() "
        " and p.code = rp.part_code "
        " and p.valid_to = ores_utility_infinity_timestamp_fn() "
        "where rp.request_id = $1::uuid "
        "  and rp.valid_to = ores_utility_infinity_timestamp_fn() "
        "order by p.answer_order, p.display_order, p.code",
        {request_id},
        lg(),
        "Reading the parts a request needs");

    std::vector<domain::approval_part> parts;
    for (const auto& row : rows) {
        if (row.empty() || !row.front())
            continue;
        if (auto found = part(*row.front()))
            parts.push_back(std::move(*found));
    }
    return parts;
}

// The rule for which parts are open is the same one
// ores_inbox_decide_approval_request_fn applies. Change them together.
std::vector<domain::approval_part>
approval_lifecycle::open_parts_of(const std::string& request_id) {
    const auto rows = ores::database::repository::execute_parameterized_multi_column_query(
        ctx_,
        "with waiting as ( "
        "  select rp.part_code, p.answer_order, p.display_order "
        "  from ores_inbox_approval_request_parts_tbl rp "
        "  join ores_inbox_approval_parts_tbl p "
        "    on p.tenant_id = ores_utility_system_tenant_id_fn() "
        "   and p.code = rp.part_code "
        "   and p.valid_to = ores_utility_infinity_timestamp_fn() "
        "  where rp.request_id = $1::uuid "
        "    and rp.valid_to = ores_utility_infinity_timestamp_fn() "
        "    and not exists ( "
        "      select 1 from ores_inbox_approval_decisions_tbl d "
        "      where d.request_id = rp.request_id "
        "        and d.part_code = rp.part_code "
        "        and d.decision_code = 'approve' "
        "        and d.valid_to = ores_utility_infinity_timestamp_fn())) "
        "select part_code from waiting "
        "where answer_order = (select min(answer_order) from waiting) "
        "order by display_order, part_code",
        {request_id},
        lg(),
        "Reading the parts a request waits on");

    std::vector<domain::approval_part> parts;
    for (const auto& row : rows) {
        if (row.empty() || !row.front())
            continue;
        if (auto found = part(*row.front()))
            parts.push_back(std::move(*found));
    }
    return parts;
}

decision_result approval_lifecycle::decide(const std::string& request_id,
                                           int version,
                                           const std::string& decision_code,
                                           const boost::uuids::uuid& decided_by,
                                           const std::string& comment,
                                           const std::string& part_code) {
    BOOST_LOG_SEV(lg(), info) << "Deciding request " << request_id << ": " << decision_code;
    const auto rows = ores::database::repository::execute_parameterized_multi_column_query(
        ctx_,
        "select outcome, message, state_code, version::text "
        "from ores_inbox_decide_approval_request_fn($1::uuid, $2::integer, $3, $4::uuid, $5, $6, nullif($7, ''))",
        {request_id,
         std::to_string(version),
         decision_code,
         boost::uuids::to_string(decided_by),
         comment,
         ctx_.actor(),
         part_code},
        lg(),
        "Deciding an approval request");

    if (rows.empty() || rows.front().size() != 4)
        throw std::runtime_error("The decision returned no result.");

    const auto& row = rows.front();
    return decision_result{.outcome = row[0].value_or(""),
                           .message = row[1].value_or(""),
                           .state_code = row[2].value_or(""),
                           .version = row[3] ? std::stoi(*row[3]) : 0};
}

decision_result approval_lifecycle::fail_apply(const std::string& request_id,
                                               const std::string& reason) {
    BOOST_LOG_SEV(lg(), info) << "Request " << request_id << " could not be applied: " << reason;
    const auto rows = ores::database::repository::execute_parameterized_multi_column_query(
        ctx_,
        "select outcome, message, state_code, version::text "
        "from ores_inbox_fail_apply_fn($1::uuid, $2, $3)",
        {request_id, reason, ctx_.actor().empty() ? ctx_.service_account() : ctx_.actor()},
        lg(),
        "Moving an approved request to apply_failed");

    if (rows.empty() || rows.front().size() != 4)
        throw std::runtime_error("The apply failure returned no result.");

    const auto& row = rows.front();
    return decision_result{.outcome = row[0].value_or(""),
                           .message = row[1].value_or(""),
                           .state_code = row[2].value_or(""),
                           .version = row[3] ? std::stoi(*row[3]) : 0};
}

request_page approval_lifecycle::queue(const std::vector<std::string>& kind_codes,
                                       const boost::uuids::uuid& excluding,
                                       int offset,
                                       int limit) {
    if (kind_codes.empty())
        return {};

    messaging::approval_requests_filter open;
    open.kind_code_one_of = kind_codes;
    open.state_code_one_of = std::vector<std::string>{"waiting", "held"};

    auto own = open;
    own.requested_by = excluding;

    repository::approval_request_repository repo;
    const auto all = static_cast<int>(repo.get_total_request_count(ctx_, open));
    const auto mine = static_cast<int>(repo.get_total_request_count(ctx_, own));

    // A person never decides their own request, so theirs are left out. The
    // read covers the page plus every request of their own that could fall
    // inside it, and the page is cut after they are removed.
    const auto page_limit = clamp_limit(limit);
    const auto first = std::max(offset, 0);
    auto rows = repo.read_latest(ctx_,
                                 0,
                                 static_cast<std::uint32_t>(first + page_limit + mine),
                                 ores::utility::domain::order{.field = "requested_at"},
                                 open);
    std::erase_if(rows, [&](const auto& r) { return r.requested_by == excluding; });

    request_page page;
    page.total = all - mine;
    if (first < static_cast<int>(rows.size())) {
        const auto last = std::min(static_cast<int>(rows.size()), first + page_limit);
        page.requests.assign(rows.begin() + first, rows.begin() + last);
    }
    return page;
}

std::vector<domain::approval_request>
approval_lifecycle::recently_answered(const std::vector<std::string>& kind_codes,
                                      const boost::uuids::uuid& excluding,
                                      std::chrono::seconds window) {
    if (kind_codes.empty() || window <= std::chrono::seconds::zero())
        return {};

    /*
     * The two arrays are comma-joined rather than passed as PostgreSQL array
     * literals: both hold codes the model itself declares, so neither can hold
     * the separator.
     */
    std::string kinds;
    for (const auto& code : kind_codes) {
        if (!kinds.empty())
            kinds += ',';
        kinds += code;
    }
    const std::string open = "waiting,held";

    const auto rows = ores::database::repository::execute_parameterized_multi_column_query(
        ctx_,
        "select r.id::text"
        " from ores_inbox_approval_requests_tbl r"
        " where r.valid_to = ores_utility_infinity_timestamp_fn()"
        " and r.kind_code = any(string_to_array($1::text, ','))"
        " and r.state_code <> all(string_to_array($2::text, ','))"
        " and r.requested_by <> $3::uuid"
        " and r.valid_from >= clock_timestamp() - make_interval(secs => $4::double precision)"
        " order by r.valid_from desc"
        " limit $5::int",
        {kinds,
         open,
         boost::uuids::to_string(excluding),
         std::to_string(window.count()),
         std::to_string(answered_tail_limit)},
        lg(),
        "Reading the approval requests answered recently");

    std::vector<std::string> ids;
    ids.reserve(rows.size());
    for (const auto& row : rows) {
        if (!row.empty() && row.front())
            ids.push_back(*row.front());
    }
    if (ids.empty())
        return {};

    repository::approval_request_repository repo;
    auto answered = repo.read_latest(ctx_, ids);
    // The store orders a read by key, so the answer's own order is put back.
    std::ranges::sort(answered, std::greater{}, &domain::approval_request::recorded_at);
    return answered;
}

request_page
approval_lifecycle::raised_by(const boost::uuids::uuid& account_id, int offset, int limit) {
    messaging::approval_requests_filter mine;
    mine.requested_by = account_id;

    repository::approval_request_repository repo;
    request_page page;
    page.total = static_cast<int>(repo.get_total_request_count(ctx_, mine));
    page.requests =
        repo.read_latest(ctx_,
                         static_cast<std::uint32_t>(std::max(offset, 0)),
                         static_cast<std::uint32_t>(clamp_limit(limit)),
                         ores::utility::domain::order{.field = "requested_at", .descending = true},
                         mine);
    return page;
}

std::vector<expired_request> approval_lifecycle::expire_overdue() {
    // The sweep runs from the scheduler, so no person is asking and there is no
    // actor to name. The store still requires one, because the row it writes
    // records who closed the request, and the honest answer is the service that
    // ran the sweep.
    const auto rows = ores::database::repository::execute_parameterized_multi_column_query(
        ctx_,
        "select request_id::text, tenant_id::text, kind_code, requested_by::text"
        " from ores_inbox_expire_approval_requests_fn($1)",
        {ctx_.service_account()},
        lg(),
        "Closing the approval requests nobody answered");

    std::vector<expired_request> expired;
    expired.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() != 4)
            continue;
        expired_request e{.request_id = row[0].value_or(""),
                          .tenant_id = row[1].value_or(""),
                          .kind_code = row[2].value_or(""),
                          .requested_by = row[3].value_or("")};
        BOOST_LOG_SEV(lg(), info) << "Request " << e.request_id << " (" << e.kind_code
                                  << ") expired undecided.";
        tell_expired(e);
        expired.push_back(std::move(e));
    }
    return expired;
}

std::vector<expiring_request> approval_lifecycle::remind_expiring(std::chrono::seconds window) {
    if (window <= std::chrono::seconds::zero())
        return {};

    // The sweep runs from the scheduler, so no person is asking. The read
    // crosses every tenant and touches no row, so it needs no actor; the notice
    // it raises needs one, and the service that ran the sweep is the honest
    // answer.
    const auto rows = ores::database::repository::execute_parameterized_multi_column_query(
        ctx_,
        "select request_id::text, tenant_id::text, kind_code, requested_by::text,"
        " expires_at::text from ores_inbox_remind_expiring_approval_requests_fn($1::double "
        "precision)",
        {std::to_string(window.count())},
        lg(),
        "Reading the approval requests close to their deadline");

    std::vector<expiring_request> expiring;
    expiring.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() != 5)
            continue;
        expiring_request e{.request_id = row[0].value_or(""),
                           .tenant_id = row[1].value_or(""),
                           .kind_code = row[2].value_or(""),
                           .requested_by = row[3].value_or(""),
                           .expires_at = row[4].value_or("")};
        BOOST_LOG_SEV(lg(), info) << "Request " << e.request_id << " (" << e.kind_code
                                  << ") is close to its deadline at " << e.expires_at << ".";
        tell_expiring(e);
        expiring.push_back(std::move(e));
    }
    return expiring;
}

void approval_lifecycle::tell_expiring(const expiring_request& expiring) {
    try {
        const auto tenant = utility::uuid::tenant_id::from_string(expiring.tenant_id);
        if (!tenant)
            throw std::runtime_error("bad tenant " + expiring.tenant_id);
        const auto k = kind(expiring.kind_code);
        if (!k)
            throw std::runtime_error("no such kind " + expiring.kind_code);

        // The sweep runs as a service account that lives in the system tenant,
        // while the people to warn live in the request's tenant. The deciders
        // are resolved where they are and the notice is raised there, so
        // warning one tenant's deciders does not need the service to hold their
        // permission.
        const auto system_ctx =
            ctx_.with_tenant(utility::uuid::tenant_id::system(), ctx_.service_account());
        const auto raiser = notification_center(system_ctx).actor_account_id();
        if (!raiser)
            throw std::runtime_error("the service account was not found");

        notification_center center(ctx_.with_tenant(*tenant, ctx_.service_account()));
        auto deciders = center.holders_of(k->decide_permission_code);
        // A person never decides their own request, and the warning is that
        // somebody should, so the one person it must not reach is the asker.
        std::erase(deciders, expiring.requested_by);
        if (deciders.empty())
            return;

        // The message names the kind as a person reads it, which is a name and
        // not the code the row carries.
        messaging::raise_notification_request n{
            .kind_code = "inbox.approval_expiring",
            .link_route = "requests",
            .link_id = expiring.request_id,
            .arguments = {{.name = "kind", .value = k->name},
                          {.name = "deadline", .value = expiring.expires_at}},
            .account_ids = {},
            .audience_permission_code = k->decide_permission_code};
        center.raise(n, deciders, *raiser);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Request " << expiring.request_id
                                  << " is close to its deadline, but its deciders were not told: "
                                  << e.what();
    }
}

void approval_lifecycle::tell_expired(const expired_request& expired) {
    try {
        const auto tenant = utility::uuid::tenant_id::from_string(expired.tenant_id);
        if (!tenant)
            throw std::runtime_error("bad tenant " + expired.tenant_id);
        // The service account that runs the sweep lives in the system tenant,
        // while the person to tell lives in theirs. The raiser is resolved
        // where it lives, and the notice is raised where the person is, so a
        // sweep of one tenant's request does not need the service to have an
        // account in that tenant.
        const auto system_ctx =
            ctx_.with_tenant(utility::uuid::tenant_id::system(), ctx_.service_account());
        const auto raiser = notification_center(system_ctx).actor_account_id();
        if (!raiser)
            throw std::runtime_error("the service account was not found");

        // The message names the kind as a person reads it, which is a name and
        // not the code the row carries.
        const auto k = kind(expired.kind_code);
        messaging::raise_notification_request n{
            .kind_code = "inbox.approval_expired",
            .link_route = "requests",
            .link_id = expired.request_id,
            .arguments = {{.name = "kind", .value = k ? k->name : expired.kind_code}},
            .account_ids = {expired.requested_by},
            .audience_permission_code = ""};
        notification_center(ctx_.with_tenant(*tenant, ctx_.service_account()))
            .raise(n, n.account_ids, *raiser);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Request " << expired.request_id
                                  << " expired, but the person who asked was not told: "
                                  << e.what();
    }
}

}
