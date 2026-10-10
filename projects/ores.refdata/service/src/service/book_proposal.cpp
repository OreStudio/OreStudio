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
#include "ores.refdata.service/service/book_proposal.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.inbox.core/service/approval_announcer.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.refdata.core/repository/book_change_repository.hpp"
#include "ores.refdata.core/service/book_service.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <algorithm>
#include <exception>
#include <stdexcept>

namespace ores::refdata::service {

using namespace ores::logging;
namespace db = ores::database::repository;
using ores::utility::domain::outcome;
using ores::utility::domain::precondition;
using ores::utility::domain::precondition_kind;

namespace {

constexpr auto kind_code = "refdata.book_change";
constexpr auto entity_type = "book";
constexpr auto savepoint = "book_line";

auto& log() {
    static auto instance = ores::logging::make_logger("ores.refdata.service.book_proposal");
    return instance;
}

/**
 * @brief Runs a statement on the connection of the context's transaction.
 *
 * The raw command helper opens a connection of its own, so a savepoint it
 * opened would not exist on the one the writes run on.
 */
void in_transaction(const ores::database::context& ctx, const std::string& sql) {
    const auto r = (*ctx.active_transaction())->execute(sql);
    if (!r)
        throw std::runtime_error(r.error().what());
}

/**
 * @brief A column of the book and whether a line changes it.
 *
 * The names are the columns the policy names, so the policy and the
 * comparison read one vocabulary.
 */
struct column {
    const char* name;
    bool (*differs)(const domain::book&, const domain::book_change&);
};

const column columns[] = {
    {"party_id", [](const auto& b, const auto& l) { return b.party_id != l.party_id; }},
    {"name", [](const auto& b, const auto& l) { return b.name != l.name; }},
    {"description", [](const auto& b, const auto& l) { return b.description != l.description; }},
    {"parent_portfolio_id",
     [](const auto& b, const auto& l) { return b.parent_portfolio_id != l.parent_portfolio_id; }},
    {"owner_unit_id", [](const auto& b, const auto& l) { return b.owner_unit_id != l.owner_unit_id; }},
    {"functional_currency",
     [](const auto& b, const auto& l) { return b.functional_currency != l.functional_currency; }},
    {"gl_account_ref", [](const auto& b, const auto& l) { return b.gl_account_ref != l.gl_account_ref; }},
    {"cost_center", [](const auto& b, const auto& l) { return b.cost_center != l.cost_center; }},
    {"book_status", [](const auto& b, const auto& l) { return b.book_status != l.book_status; }},
    {"regulatory_book_type",
     [](const auto& b, const auto& l) { return b.regulatory_book_type != l.regulatory_book_type; }},
    {"book_purpose_type",
     [](const auto& b, const auto& l) { return b.book_purpose_type != l.book_purpose_type; }},
    {"ledger_feed_type",
     [](const auto& b, const auto& l) { return b.ledger_feed_type != l.ledger_feed_type; }},
    {"is_sweepable", [](const auto& b, const auto& l) { return b.is_sweepable != l.is_sweepable; }},
    {"rates_centre_code",
     [](const auto& b, const auto& l) { return b.rates_centre_code != l.rates_centre_code; }},
    {"sandbox_id", [](const auto& b, const auto& l) { return b.sandbox_id != l.sandbox_id; }},
};

/**
 * @brief The columns a line changes.
 *
 * A line that creates a row changes every column. A delete changes none: the
 * policy reads the operation alone.
 */
std::vector<std::string> changed_columns(const std::optional<domain::book>& live,
                                         const domain::book_change& line) {
    std::vector<std::string> names;
    if (line.operation == "delete")
        return names;
    for (const auto& c : columns) {
        if (!live || c.differs(*live, line))
            names.push_back(c.name);
    }
    return names;
}

messaging::book_write to_write(const domain::book_change& l) {
    return {.id = l.entity_id,
            .party_id = l.party_id,
            .name = l.name,
            .description = l.description,
            .parent_portfolio_id = l.parent_portfolio_id,
            .owner_unit_id = l.owner_unit_id,
            .functional_currency = l.functional_currency,
            .gl_account_ref = l.gl_account_ref,
            .cost_center = l.cost_center,
            .book_status = l.book_status,
            .regulatory_book_type = l.regulatory_book_type,
            .book_purpose_type = l.book_purpose_type,
            .ledger_feed_type = l.ledger_feed_type,
            .is_sweepable = l.is_sweepable,
            .rates_centre_code = l.rates_centre_code,
            .sandbox_id = l.sandbox_id};
}

/**
 * @brief The claim a line makes about the live row, as the real write reads it.
 *
 * A base version of zero claims the row is new. Any other claims the version
 * the maker read.
 */
precondition claim_of(const domain::book_change& l) {
    if (l.base_version == 0)
        return precondition{precondition_kind::must_not_exist, std::nullopt};
    return precondition{precondition_kind::must_match_version,
                        static_cast<std::uint32_t>(l.base_version)};
}

std::string refusal_of(const ores::utility::domain::result& r) {
    if (r.outcome == outcome::ok)
        return {};
    return r.message.empty() ? r.code : r.message;
}

/**
 * @brief Runs the real write for one line and returns its refusal, if any.
 */
std::string run_write(book_service& svc,
                      const std::optional<domain::book>& live,
                      const domain::book_change& line,
                      const ores::utility::domain::change_intent& intent) {
    if (line.operation == "put") {
        messaging::put_book_request req;
        req.change.write = to_write(line);
        req.change.precondition = claim_of(line);
        req.intent = intent;
        return refusal_of(svc.put_book(req).result);
    }
    if (line.operation == "delete") {
        if (!live)
            return "No book with this id to remove.";
        messaging::delete_book_request req;
        req.removal.key.name = live->name;
        if (line.base_version != 0) {
            req.removal.precondition = precondition{precondition_kind::must_match_version,
                                                    static_cast<std::uint32_t>(line.base_version)};
        }
        req.intent = intent;
        return refusal_of(svc.delete_book(req).result);
    }
    return "The operation must be put or delete: " + line.operation;
}

/**
 * @brief The parts the policy names for an act, read as the caller.
 *
 * The helper opens its own connection and sets the caller's tenant and party
 * on it, so the tenant's own policy rows are read as well as the system's.
 * Every value is a bound parameter: the operation comes from the caller.
 */
std::vector<std::string> policy_parts(const ores::database::context& ctx,
                                      const std::string& operation,
                                      const std::vector<std::string>& cols) {
    std::string list = "{";
    for (std::size_t i = 0; i < cols.size(); ++i)
        list += (i ? ",\"" : "\"") + cols[i] + "\"";
    list += "}";

    std::vector<std::string> codes;
    const auto rows = db::execute_parameterized_multi_column_query(
        ctx,
        "select p from ores_inbox_policy_parts_fn($1, $2, $3::text[]) as p",
        {entity_type, operation, list},
        log(),
        "Reading the parts the policy names");
    for (const auto& row : rows) {
        if (!row.empty() && row[0])
            codes.push_back(*row[0]);
    }
    return codes;
}

}

bool book_preview::refused() const {
    return std::ranges::any_of(lines, [](const auto& l) { return !l.refusal.empty(); });
}

book_proposal_service::book_proposal_service(ores::database::context ctx)
    : ctx_(std::move(ctx)) {}

book_preview book_proposal_service::preview(const std::vector<domain::book_change>& lines) {
    // The transaction is never committed. Its destructor rolls back every
    // write the lines made, so the live table is left as it was and the
    // notifications the writes queued are never sent.
    db::unit_of_work uow(ctx_);
    const auto& ctx = uow.ctx();
    book_service svc(ctx);

    book_preview result;
    int line_no = 0;
    for (const auto& line : lines) {
        ++line_no;
        line_outcome out{.line_no = line_no,
                         .operation = line.operation,
                         .entity_id = line.entity_id,
                         .columns = {},
                         .refusal = {}};

        const auto live = svc.get_book(line.entity_id);
        out.columns = changed_columns(live, line);

        in_transaction(ctx, std::string("savepoint ") + savepoint);
        try {
            out.refusal = run_write(svc,
                                    live,
                                    line,
                                    {line.change_reason_code, line.change_commentary});
        } catch (const std::exception& e) {
            out.refusal = e.what();
        }
        // A failed statement aborts the transaction until the savepoint is
        // rolled back, so a refusal rolls back and the next line starts clean.
        // If the savepoint statement itself fails the transaction is
        // unusable: the exception leaves the preview and the unit of work
        // rolls back.
        in_transaction(ctx,
                       std::string(out.refusal.empty() ? "release savepoint " :
                                                         "rollback to savepoint ") +
                           savepoint);

        for (const auto& code : policy_parts(ctx, line.operation, out.columns)) {
            if (std::ranges::find(result.part_codes, code) == result.part_codes.end())
                result.part_codes.push_back(code);
        }
        result.lines.push_back(std::move(out));
    }
    std::ranges::sort(result.part_codes);
    return result;
}

bool is_infrastructure_failure(const std::string& what) {
    for (const char* marker : {"connection", "could not connect", "server closed", "timeout",
                               "timed out", "terminating", "Cannot begin a transaction",
                               "Cannot commit the transaction"}) {
        if (what.find(marker) != std::string::npos)
            return true;
    }
    return false;
}

namespace {

/**
 * @brief The stored lines of a request, in line order.
 */
std::vector<domain::book_change> lines_of(const ores::database::context& ctx,
                                          const std::string& request_id) {
    auto rows = repository::book_change_repository{}.read_latest_by_request_id(
        ctx, request_id, 0, 1000);
    std::ranges::sort(rows, {}, &domain::book_change::line_no);
    return rows;
}

}

book_preview book_proposal_service::recheck(const std::string& request_id) {
    return preview(lines_of(ctx_, request_id));
}

book_apply book_proposal_service::apply(const std::string& request_id) {
    ores::inbox::service::approval_lifecycle lifecycle(ctx_);
    const auto request = lifecycle.request(request_id);
    if (!request)
        return {.refusal = "No such request."};
    if (request->state_code != "approved") {
        return {.refusal = "The request is " + request->state_code +
                           " and cannot be applied."};
    }

    book_apply out;
    try {
        db::unit_of_work uow(ctx_);
        const auto& ctx = uow.ctx();
        book_service svc(ctx);
        repository::book_change_repository lines;

        for (auto row : lines_of(ctx, request_id)) {
            // A line applies once, so a rerun after a partial failure that
            // rolled back finds the same lines, and one that completed finds
            // none.
            if (row.applied)
                continue;

            const auto live = svc.get_book(row.entity_id);
            std::string refusal;
            try {
                const ores::utility::domain::change_intent intent{
                    row.change_reason_code,
                    "Approval request " + request_id +
                        (row.change_commentary.empty() ? "" : ": " + row.change_commentary)};
                refusal = run_write(svc, live, row, intent);
            } catch (const std::exception& e) {
                if (is_infrastructure_failure(e.what()))
                    throw;
                refusal = e.what();
            }
            if (!refusal.empty()) {
                out.failed_line = row.line_no;
                out.refusal = refusal;
                break;
            }

            row.applied = true;
            row.modified_by = ctx.actor();
            row.performed_by = ctx.service_account();
            row.change_reason_code = "system.update";
            lines.write(ctx, row);
            ++out.lines_applied;
        }

        if (out.failed_line == 0) {
            uow.commit();
            out.applied = true;
            return out;
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(log(), ores::logging::error)
            << "Applying request " << request_id << " failed: " << e.what();
        return {.transient = true, .refusal = e.what()};
    }

    // The transaction is rolled back, so nothing was written. The request ends
    // here: a refusal does not change on a retry.
    out.lines_applied = 0;
    lifecycle.fail_apply(request_id, "Line " + std::to_string(out.failed_line) + ": " + out.refusal);
    return out;
}

book_proposal book_proposal_service::raise(const std::vector<domain::book_change>& lines,
                                           const std::string& reason) {
    book_proposal out;
    if (lines.empty()) {
        out.message = "Propose at least one change.";
        return out;
    }
    if (reason.empty()) {
        out.message = "Say why you ask.";
        return out;
    }

    // The preview and the raise are separate transactions, so a book can
    // change between them. The stored base version is then stale, and the
    // apply re-checks it against the live row before it writes.
    out.preview = preview(lines);
    if (out.preview.refused()) {
        out.message = "A change was refused. Nothing was raised.";
        return out;
    }
    if (out.preview.part_codes.empty()) {
        out.message = "These changes need no request. Write them directly.";
        return out;
    }

    std::optional<ores::inbox::domain::approval_kind> kind;
    std::optional<ores::inbox::domain::approval_request> raised;
    {
        db::unit_of_work uow(ctx_);
        const auto& ctx = uow.ctx();
        ores::inbox::service::approval_lifecycle lifecycle(ctx);
        kind = lifecycle.kind(kind_code);
        if (!kind)
            throw std::runtime_error(std::string("No such kind of request: ") + kind_code);
        const auto me = lifecycle.actor_account_id();
        if (!me)
            throw std::runtime_error("The signed-in account was not found.");

        raised = lifecycle.raise(*kind, reason, *me, out.preview.part_codes);

        std::vector<domain::book_change> rows = lines;
        int line_no = 0;
        for (auto& row : rows) {
            ++line_no;
            row.tenant_id = ctx.tenant_id();
            row.id = ores::utility::uuid::uuid_v7_generator{}();
            row.request_id = raised->id;
            row.line_no = line_no;
            row.modified_by = ctx.actor();
            row.performed_by = ctx.service_account();
            // The maker's reason and commentary are the intent the apply
            // replays, so they are kept. A line with none takes the default.
            if (row.change_reason_code.empty())
                row.change_reason_code = "system.new_record";
        }
        repository::book_change_repository{}.write(ctx, rows);
        uow.commit();
    }

    // The request stands whether or not anybody is told, so the telling
    // happens after the commit and a failure there is only logged.
    ores::inbox::service::approval_lifecycle lifecycle(ctx_);
    ores::inbox::service::approval_announcer(ctx_).tell_open_parts(lifecycle, *kind, *raised);

    out.request = lifecycle.request(boost::uuids::to_string(raised->id));
    out.message = "Raised.";
    return out;
}

}
