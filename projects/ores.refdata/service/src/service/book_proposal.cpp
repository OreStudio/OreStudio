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
#include "ores.refdata.service/service/proposal_support.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include "shared_helpers.hpp"
#include <algorithm>
#include <cstdint>
#include <exception>
#include <stdexcept>

namespace ores::refdata::service {

using namespace ores::logging;
namespace db = ores::database::repository;
using ores::utility::domain::outcome;
using support::in_transaction;
using support::policy_parts;
using support::refusal_of;
using ores::utility::domain::precondition;
using ores::utility::domain::precondition_kind;

namespace {

constexpr auto kind_code = "refdata.book_change";
constexpr auto entity_type = "book";
constexpr auto savepoint = "book_line";

// The operation a policy names for the act that opens a book, as distinct from
// the act that changes one. A put of a book that does not exist yet is both, so
// its parts are the union of the two.
constexpr auto create_operation = "create";

auto& log() {
    static auto instance = ores::logging::make_logger("ores.refdata.service.book_proposal");
    return instance;
}

ores::refdata::messaging::book_write to_write(const domain::book_change& l) {
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

/**
 * @brief Runs the real write for one line and returns its refusal, if any.
 */
std::string run_write(book_service& svc,
                      const std::optional<domain::book>& live,
                      const domain::book_change& line,
                      const ores::utility::domain::change_intent& intent) {
    if (line.operation == "put") {
        ores::refdata::messaging::put_book_request req;
        req.change.write = to_write(line);
        req.change.precondition = claim_of(line);
        req.intent = intent;
        return refusal_of(svc.put_book(req).result);
    }
    if (line.operation == "delete") {
        if (!live)
            return "No book with this id to remove.";
        ores::refdata::messaging::delete_book_request req;
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
    // The writes below are rolled back, so the approval gate lets them through.
    svc.rehearse();

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
        // A delete changes no column: the policy reads the operation alone.
        if (line.operation != "delete")
            out.columns = svc.changed_columns(to_write(line));

        in_transaction(ctx, std::string("savepoint ") + savepoint);
        try {
            out.refusal =
                run_write(svc, live, line, {line.change_reason_code, line.change_commentary});
        } catch (const std::exception& e) {
            // A refusal is the database's answer. A lost connection is not an
            // answer, and reporting it as one would hide it behind a refused line.
            if (is_infrastructure_failure(e.what()))
                throw;
            out.refusal = e.what();
        }
        // A failed statement aborts the transaction until the savepoint is
        // rolled back, so a refusal rolls back and the next line starts clean.
        // If the savepoint statement itself fails the transaction is
        // unusable: the exception leaves the preview and the unit of work
        // rolls back.
        in_transaction(
            ctx,
            std::string(out.refusal.empty() ? "release savepoint " : "rollback to savepoint ") +
                savepoint);

        out.part_codes = policy_parts(ctx, entity_type, line.operation, out.columns);
        // A put of a book that is not there is the act that opens it. That act
        // asks the functions the opening chain names as well as the ones that
        // own a column, because the controller's check is of the opening and
        // not of any field the opening writes.
        if (line.operation == "put" && !live) {
            for (const auto& code : policy_parts(ctx, entity_type, create_operation, out.columns)) {
                if (std::ranges::find(out.part_codes, code) == out.part_codes.end())
                    out.part_codes.push_back(code);
            }
        }
        for (const auto& code : out.part_codes) {
            if (std::ranges::find(result.part_codes, code) == result.part_codes.end())
                result.part_codes.push_back(code);
        }
        result.lines.push_back(std::move(out));
    }
    std::ranges::sort(result.part_codes);
    return result;
}

bool is_infrastructure_failure(const std::string& what) {
    const auto refused = what.find("ERROR:  ");
    const auto ended = what.find("FATAL:  ");
    if (refused == std::string::npos && ended == std::string::npos)
        return true;
    return ended != std::string::npos && (refused == std::string::npos || ended < refused);
}

namespace {

/**
 * @brief The stored lines of a request, in line order.
 */
std::vector<domain::book_change> lines_of(const ores::database::context& ctx,
                                          const std::string& request_id) {
    // One more than the most a request may hold, so a request over the limit
    // is seen as over it and never read as a shorter one.
    auto rows = repository::book_change_repository{}.read_latest_by_request_id(
        ctx, request_id, 0, static_cast<std::uint32_t>(book_proposal_service::max_lines + 1));
    std::ranges::sort(rows, {}, &domain::book_change::line_no);
    return rows;
}

}

namespace {

/**
 * @brief A column of the book beside the same column of a line, as text.
 *
 * The names are the columns the policy names, so a screen and the policy read
 * one vocabulary.
 */
template <typename BookMember, typename LineMember>
field_delta delta_of(const char* name,
                     const std::optional<domain::book>& live,
                     BookMember book_member,
                     const domain::book_change& line,
                     LineMember line_member) {
    field_delta d;
    d.name = name;
    d.after = text_of(line.*line_member);
    if (live)
        d.before = text_of((*live).*book_member);
    d.changed = !live || d.before != d.after;
    return d;
}

}

std::vector<line_delta> book_proposal_service::deltas(const std::string& request_id) {
    const auto rows = lines_of(ctx_, request_id);
    book_service svc(ctx_);

    std::vector<line_delta> out;
    out.reserve(rows.size());
    for (const auto& line : rows) {
        line_delta d{.line_no = line.line_no,
                     .operation = line.operation,
                     .entity_id = line.entity_id,
                     .base_version = line.base_version,
                     .applied = line.applied,
                     .fields = {}};
        if (line.operation == "put") {
            // The book as the maker read it, so a line that has applied still
            // shows what it changed. A new book has nothing before it.
            std::optional<domain::book> live;
            if (line.base_version > 0) {
                live = svc.get_book_at_version(line.entity_id,
                                               static_cast<std::uint32_t>(line.base_version));
            }
            using B = domain::book;
            using L = domain::book_change;
            d.fields = {
                delta_of("party_id", live, &B::party_id, line, &L::party_id),
                delta_of("name", live, &B::name, line, &L::name),
                delta_of("description", live, &B::description, line, &L::description),
                delta_of("parent_portfolio_id",
                         live,
                         &B::parent_portfolio_id,
                         line,
                         &L::parent_portfolio_id),
                delta_of("owner_unit_id", live, &B::owner_unit_id, line, &L::owner_unit_id),
                delta_of("functional_currency",
                         live,
                         &B::functional_currency,
                         line,
                         &L::functional_currency),
                delta_of("gl_account_ref", live, &B::gl_account_ref, line, &L::gl_account_ref),
                delta_of("cost_center", live, &B::cost_center, line, &L::cost_center),
                delta_of("book_status", live, &B::book_status, line, &L::book_status),
                delta_of("regulatory_book_type",
                         live,
                         &B::regulatory_book_type,
                         line,
                         &L::regulatory_book_type),
                delta_of(
                    "book_purpose_type", live, &B::book_purpose_type, line, &L::book_purpose_type),
                delta_of(
                    "ledger_feed_type", live, &B::ledger_feed_type, line, &L::ledger_feed_type),
                delta_of("is_sweepable", live, &B::is_sweepable, line, &L::is_sweepable),
                delta_of(
                    "rates_centre_code", live, &B::rates_centre_code, line, &L::rates_centre_code),
                delta_of("sandbox_id", live, &B::sandbox_id, line, &L::sandbox_id),
            };
        }
        out.push_back(std::move(d));
    }
    return out;
}

book_preview book_proposal_service::recheck(const std::string& request_id) {
    const auto rows = lines_of(ctx_, request_id);
    if (rows.size() > max_lines)
        throw std::runtime_error("The request holds more lines than a request may hold.");
    return preview(rows);
}

book_apply book_proposal_service::apply(const std::string& request_id) {
    ores::inbox::service::approval_lifecycle lifecycle(ctx_);
    const auto request = lifecycle.request(request_id);
    if (!request)
        return {.refusal = "No such request."};
    if (request->state_code != "approved") {
        return {.refusal = "The request is " + request->state_code + " and cannot be applied."};
    }

    book_apply out;
    try {
        db::unit_of_work uow(ctx_);
        const auto& ctx = uow.ctx();

        // Two applies of one request must not both write. The request row is
        // locked for the transaction, and its state is read again under the
        // lock, because the read above was made before it.
        in_transaction(ctx, support::lock_request_sql(request_id));
        const auto locked = ores::inbox::service::approval_lifecycle(ctx).request(request_id);
        if (!locked || locked->state_code != "approved") {
            return {.refusal = "The request is " +
                               (locked ? locked->state_code : std::string("gone")) +
                               " and cannot be applied."};
        }

        const auto stored = lines_of(ctx, request_id);
        if (stored.size() > max_lines) {
            out.refusal = "The request holds more lines than a request may hold.";
            out.failed_line = 1;
            throw std::length_error(out.refusal);
        }

        book_service svc(ctx);
        // The gate lets a write through only when this approved request holds
        // the line the write carries out.
        svc.carry_out_request(request_id);
        repository::book_change_repository lines;

        for (auto row : stored) {
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
            } catch (const std::logic_error& e) {
                // The service refused the line's content before it reached the
                // store, and a retry would meet the same answer.
                refusal = e.what();
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
    } catch (const std::length_error& e) {
        // The request is over the limit: a retry reads the same lines.
        out.lines_applied = 0;
        out.refusal = e.what();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(log(), ores::logging::error)
            << "Applying request " << request_id << " failed: " << e.what();
        return {.transient = true, .refusal = e.what()};
    }

    // The transaction is rolled back, so nothing was written. The request ends
    // here: a refusal does not change on a retry.
    out.lines_applied = 0;
    try {
        const auto moved = lifecycle.fail_apply(
            request_id, "Line " + std::to_string(out.failed_line) + ": " + out.refusal);
        if (moved.outcome != "ok") {
            BOOST_LOG_SEV(log(), ores::logging::warn)
                << "Request " << request_id << " was refused but is " << moved.state_code
                << ", not apply_failed: " << moved.message;
        }
    } catch (const std::exception& e) {
        // Nothing was written and the request is still approved, so the apply
        // may run again and meet the same refusal.
        BOOST_LOG_SEV(log(), ores::logging::error)
            << "Request " << request_id << " could not be moved to apply_failed: " << e.what();
        return {.transient = true, .refusal = e.what()};
    }
    return out;
}

book_proposal book_proposal_service::raise(const std::vector<domain::book_change>& lines,
                                           const std::string& reason) {
    book_proposal out;
    if (lines.empty()) {
        out.message = "Propose at least one change.";
        return out;
    }
    if (lines.size() > max_lines) {
        out.message = "A request holds at most " + std::to_string(max_lines) + " changes.";
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
