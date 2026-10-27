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
#include "ores.refdata.service/service/counterparty_proposal.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.inbox.core/service/approval_announcer.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.refdata.api/messaging/counterparty_identifier_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/party_counterparty_protocol.hpp"
#include "ores.refdata.core/repository/counterparty_change_repository.hpp"
#include "ores.refdata.core/repository/counterparty_identifier_change_repository.hpp"
#include "ores.refdata.core/repository/party_counterparty_change_repository.hpp"
#include "ores.refdata.core/service/counterparty_identifier_service.hpp"
#include "ores.refdata.core/service/counterparty_service.hpp"
#include "ores.refdata.core/service/party_counterparty_service.hpp"
#include "ores.refdata.service/service/proposal_support.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include "shared_helpers.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstdint>
#include <exception>
#include <stdexcept>
#include <string_view>

namespace ores::refdata::service {

using namespace ores::logging;
namespace db = ores::database::repository;
using ores::utility::domain::change_intent;
using ores::utility::domain::precondition;
using ores::utility::domain::precondition_kind;
using support::in_transaction;
using support::policy_parts;
using support::refusal_of;

namespace {

/**
 * @brief Opens the approval gate of a service for the writes that follow.
 *
 * The gate refuses a write that no approved request carries. An apply names its
 * request. A preview names none and is a rehearsal, because its transaction is
 * never committed.
 */
template <typename Service>
void open_gate(Service& svc, const std::string& request_id) {
    if (request_id.empty())
        svc.rehearse();
    else
        svc.carry_out_request(request_id);
}

constexpr auto counterparty_proposal_kind_code = "refdata.counterparty_change";
constexpr auto counterparty_proposal_savepoint = "counterparty_line";

auto& counterparty_proposal_log() {
    static auto instance = ores::logging::make_logger("ores.refdata.service.counterparty_proposal");
    return instance;
}

/**
 * @brief A column of a row beside the same column of a line, as text.
 *
 * The names are the columns the policy names, so a screen and the policy read
 * one vocabulary.
 */
template <typename Row, typename RowMember, typename Line, typename LineMember>
field_delta counterparty_proposal_delta_of(const char* name,
                                           const std::optional<Row>& live,
                                           RowMember row_member,
                                           const Line& line,
                                           LineMember line_member) {
    field_delta d;
    d.name = name;
    d.after = text_of(line.*line_member);
    if (live)
        d.before = text_of((*live).*row_member);
    d.changed = !live || d.before != d.after;
    return d;
}

/**
 * @brief The claim a line makes about the live row, as the real write reads it.
 *
 * A base version of zero claims the row is new. Any other claims the version the
 * maker read.
 */
precondition counterparty_proposal_claim_of(int base_version) {
    if (base_version == 0)
        return precondition{precondition_kind::must_not_exist, std::nullopt};
    return precondition{precondition_kind::must_match_version,
                        static_cast<std::uint32_t>(base_version)};
}

std::optional<precondition> removal_claim(int base_version) {
    if (base_version == 0)
        return std::nullopt;
    return precondition{precondition_kind::must_match_version,
                        static_cast<std::uint32_t>(base_version)};
}

std::vector<std::string> columns_of(const std::vector<field_delta>& fields) {
    std::vector<std::string> names;
    for (const auto& f : fields) {
        if (f.changed)
            names.push_back(f.name);
    }
    return names;
}

/**
 * @brief The counterparty itself.
 *
 * A kind describes how a request treats one kind of row: the live row a line
 * names, the columns it changes, and the real write that carries it out. The
 * rest of the service works on any kind through these.
 */
struct counterparty_kind {
    using line = domain::counterparty_change;
    using row = domain::counterparty;
    using repo = repository::counterparty_change_repository;

    static constexpr auto type = "counterparty";
    static constexpr auto policy = "counterparty";

    static std::string subject(const line& l) {
        return text_of(l.entity_id);
    }

    static std::optional<row> live(const ores::database::context& ctx, const line& l) {
        return counterparty_service(ctx).get_counterparty(l.entity_id);
    }

    static std::optional<row> at_version(const ores::database::context& ctx, const line& l) {
        if (l.base_version <= 0)
            return std::nullopt;
        return counterparty_service(ctx).get_counterparty_at_version(
            l.entity_id, static_cast<std::uint32_t>(l.base_version));
    }

    static std::vector<field_delta> fields(const std::optional<row>& before, const line& l) {
        using R = domain::counterparty;
        using L = domain::counterparty_change;
        return {
            counterparty_proposal_delta_of("short_code", before, &R::short_code, l, &L::short_code),
            counterparty_proposal_delta_of("full_name", before, &R::full_name, l, &L::full_name),
            counterparty_proposal_delta_of(
                "transliterated_name", before, &R::transliterated_name, l, &L::transliterated_name),
            counterparty_proposal_delta_of("party_type", before, &R::party_type, l, &L::party_type),
            counterparty_proposal_delta_of("parent_counterparty_id",
                                           before,
                                           &R::parent_counterparty_id,
                                           l,
                                           &L::parent_counterparty_id),
            counterparty_proposal_delta_of("status", before, &R::status, l, &L::status),
            counterparty_proposal_delta_of("image_id", before, &R::image_id, l, &L::image_id),
        };
    }

    static std::string write(const ores::database::context& ctx,
                             const std::optional<row>& live,
                             const line& l,
                             const change_intent& intent,
                             const std::string& request_id) {
        counterparty_service svc(ctx);
        open_gate(svc, request_id);
        if (l.operation == "put") {
            ores::refdata::messaging::put_counterparty_request req;
            req.change.write = {.id = l.entity_id,
                                .short_code = l.short_code,
                                .full_name = l.full_name,
                                .transliterated_name = l.transliterated_name,
                                .party_type = l.party_type,
                                .parent_counterparty_id = l.parent_counterparty_id,
                                .status = l.status,
                                .image_id = l.image_id};
            req.change.precondition = counterparty_proposal_claim_of(l.base_version);
            req.intent = intent;
            return refusal_of(svc.put_counterparty(req).result);
        }
        if (!live)
            return "No counterparty with this id to remove.";
        ores::refdata::messaging::delete_counterparty_request req;
        req.removal.key.short_code = live->short_code;
        if (const auto claim = removal_claim(l.base_version))
            req.removal.precondition = *claim;
        req.intent = intent;
        return refusal_of(svc.delete_counterparty(req).result);
    }
};

/**
 * @brief An identifier of a counterparty.
 */
struct identifier_kind {
    using line = domain::counterparty_identifier_change;
    using row = domain::counterparty_identifier;
    using repo = repository::counterparty_identifier_change_repository;

    static constexpr auto type = "counterparty_identifier";
    static constexpr auto policy = "counterparty_identifier";

    static std::string subject(const line& l) {
        return text_of(l.entity_id);
    }

    static std::optional<row> live(const ores::database::context& ctx, const line& l) {
        return counterparty_identifier_service(ctx).get_counterparty_identifier(l.entity_id);
    }

    static std::optional<row> at_version(const ores::database::context& ctx, const line& l) {
        if (l.base_version <= 0)
            return std::nullopt;
        return counterparty_identifier_service(ctx).get_counterparty_identifier_at_version(
            l.entity_id, static_cast<std::uint32_t>(l.base_version));
    }

    static std::vector<field_delta> fields(const std::optional<row>& before, const line& l) {
        using R = domain::counterparty_identifier;
        using L = domain::counterparty_identifier_change;
        return {
            counterparty_proposal_delta_of(
                "counterparty_id", before, &R::counterparty_id, l, &L::counterparty_id),
            counterparty_proposal_delta_of("id_scheme", before, &R::id_scheme, l, &L::id_scheme),
            counterparty_proposal_delta_of("id_value", before, &R::id_value, l, &L::id_value),
            counterparty_proposal_delta_of(
                "description", before, &R::description, l, &L::description),
            counterparty_proposal_delta_of(
                "is_authoritative", before, &R::is_authoritative, l, &L::is_authoritative),
        };
    }

    static std::string write(const ores::database::context& ctx,
                             const std::optional<row>& live,
                             const line& l,
                             const change_intent& intent,
                             const std::string& request_id) {
        counterparty_identifier_service svc(ctx);
        open_gate(svc, request_id);
        if (l.operation == "put") {
            ores::refdata::messaging::put_counterparty_identifier_request req;
            req.change.write = {.id = l.entity_id,
                                .counterparty_id = l.counterparty_id,
                                .id_scheme = l.id_scheme,
                                .id_value = l.id_value,
                                .description = l.description,
                                .is_authoritative = l.is_authoritative};
            req.change.precondition = counterparty_proposal_claim_of(l.base_version);
            req.intent = intent;
            return refusal_of(svc.put_counterparty_identifier(req).result);
        }
        if (!live)
            return "No identifier with this id to remove.";
        ores::refdata::messaging::delete_counterparty_identifier_request req;
        req.removal.key.id_value = live->id_value;
        if (const auto claim = removal_claim(l.base_version))
            req.removal.precondition = *claim;
        req.intent = intent;
        return refusal_of(svc.delete_counterparty_identifier(req).result);
    }
};

/**
 * @brief The link that lets a party trade with a counterparty.
 *
 * A link is its two halves and nothing else, so a line makes it or removes it
 * whole and names no columns of its own to compare.
 */
struct link_kind {
    using line = domain::party_counterparty_change;
    using row = domain::party_counterparty;
    using repo = repository::party_counterparty_change_repository;

    static constexpr auto type = "party_counterparty";
    static constexpr auto policy = "party_counterparty";

    static std::string subject(const line& l) {
        return text_of(l.party_id) + " / " + text_of(l.counterparty_id);
    }

    static std::optional<row> live(const ores::database::context& ctx, const line& l) {
        ores::refdata::messaging::get_party_counterparty_request req;
        req.key = {.party_id = l.party_id, .counterparty_id = l.counterparty_id};
        return party_counterparty_service(ctx).get_party_counterparty(req).party_counterparty;
    }

    static std::optional<row> at_version(const ores::database::context&, const line&) {
        return std::nullopt;
    }

    static std::vector<field_delta> fields(const std::optional<row>& before, const line& l) {
        using R = domain::party_counterparty;
        using L = domain::party_counterparty_change;
        return {
            counterparty_proposal_delta_of("party_id", before, &R::party_id, l, &L::party_id),
            counterparty_proposal_delta_of(
                "counterparty_id", before, &R::counterparty_id, l, &L::counterparty_id),
        };
    }

    static std::string write(const ores::database::context& ctx,
                             const std::optional<row>& live,
                             const line& l,
                             const change_intent& intent,
                             const std::string& request_id) {
        party_counterparty_service svc(ctx);
        open_gate(svc, request_id);
        if (l.operation == "put") {
            ores::refdata::messaging::put_party_counterparty_request req;
            req.change.write = {.party_id = l.party_id, .counterparty_id = l.counterparty_id};
            req.change.precondition = counterparty_proposal_claim_of(l.base_version);
            req.intent = intent;
            return refusal_of(svc.put_party_counterparty(req).result);
        }
        if (!live)
            return "No such link to remove.";
        ores::refdata::messaging::delete_party_counterparty_request req;
        req.removal.key = {.party_id = l.party_id, .counterparty_id = l.counterparty_id};
        if (const auto claim = removal_claim(l.base_version))
            req.removal.precondition = *claim;
        req.intent = intent;
        return refusal_of(svc.delete_party_counterparty(req).result);
    }
};

/**
 * @brief The stored lines of a request of one kind, in line order.
 */
template <typename Kind>
std::vector<typename Kind::line> counterparty_proposal_lines_of(const ores::database::context& ctx,
                                                                const std::string& request_id) {
    // One more than the most a request may hold, so a request over the limit is
    // seen as over it and never read as a shorter one. The limit is on the sum over
    // the three kinds, which the caller checks, so a request can read up to three
    // times the limit before that check refuses it.
    auto rows = typename Kind::repo{}.read_latest_by_request_id(
        ctx,
        request_id,
        0,
        static_cast<std::uint32_t>(counterparty_proposal_service::max_lines + 1));
    std::ranges::sort(rows, {}, &Kind::line::line_no);
    return rows;
}

/**
 * @brief Runs the lines of one kind and one operation through the real write
 * inside the preview's transaction, one savepoint each, and adds the outcomes.
 *
 * A line keeps the number it has among all the lines of the request, which is the
 * first number of its kind plus its place in the kind, whatever the order the
 * passes run in.
 */
template <typename Kind>
void preview_kind(const ores::database::context& ctx,
                  const std::vector<typename Kind::line>& lines,
                  int first_line_no,
                  std::string_view only,
                  counterparty_preview& result) {
    int line_no = first_line_no - 1;
    for (const auto& line : lines) {
        ++line_no;
        if (line.operation != only)
            continue;
        counterparty_line_outcome out{.line_no = line_no,
                                      .entity_type = Kind::type,
                                      .operation = line.operation,
                                      .subject_id = Kind::subject(line),
                                      .columns = {},
                                      .refusal = {},
                                      .part_codes = {}};

        const auto live = Kind::live(ctx, line);
        // A delete changes no column: the policy reads the operation alone.
        if (line.operation == "put")
            out.columns = columns_of(Kind::fields(live, line));

        in_transaction(ctx, std::string("savepoint ") + counterparty_proposal_savepoint);
        try {
            out.refusal =
                Kind::write(ctx, live, line, {line.change_reason_code, line.change_commentary}, {});
        } catch (const std::exception& e) {
            // A refusal is the database's answer. A lost connection is not an
            // answer, and reporting it as one would hide it behind a refused line.
            if (is_infrastructure_failure(e.what()))
                throw;
            out.refusal = e.what();
        }
        // A failed statement aborts the transaction until the savepoint is rolled
        // back, so a refusal rolls back and the next line starts clean.
        in_transaction(
            ctx,
            std::string(out.refusal.empty() ? "release savepoint " : "rollback to savepoint ") +
                counterparty_proposal_savepoint);

        out.part_codes = policy_parts(ctx, Kind::policy, line.operation, out.columns);
        for (const auto& code : out.part_codes) {
            if (std::ranges::find(result.part_codes, code) == result.part_codes.end())
                result.part_codes.push_back(code);
        }
        result.lines.push_back(std::move(out));
    }
}

/**
 * @brief Applies the unapplied stored lines of one kind in the apply's
 * transaction, and marks each applied.
 *
 * @return Whether a line was refused, which ends the apply.
 */
template <typename Kind>
bool apply_kind(const ores::database::context& ctx,
                const std::string& request_id,
                std::vector<typename Kind::line> stored,
                std::string_view only,
                counterparty_apply& out) {
    typename Kind::repo lines;
    for (auto row : stored) {
        if (row.operation != only)
            continue;
        // A line applies once, so a rerun after a partial failure that rolled
        // back finds the same lines, and one that completed finds none.
        if (row.applied)
            continue;

        const auto live = Kind::live(ctx, row);
        std::string refusal;
        try {
            const change_intent intent{
                row.change_reason_code,
                "Approval request " + request_id +
                    (row.change_commentary.empty() ? "" : ": " + row.change_commentary)};
            refusal = Kind::write(ctx, live, row, intent, request_id);
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
            return true;
        }

        row.applied = true;
        row.modified_by = ctx.actor();
        row.performed_by = ctx.service_account();
        row.change_reason_code = "system.update";
        lines.write(ctx, row);
        ++out.lines_applied;
    }
    return false;
}

/**
 * @brief The stored lines of one kind as a decider reads them.
 */
template <typename Kind>
void deltas_kind(const ores::database::context& ctx,
                 const std::string& request_id,
                 std::vector<counterparty_line_delta>& out) {
    for (const auto& line : counterparty_proposal_lines_of<Kind>(ctx, request_id)) {
        counterparty_line_delta d{.line_no = line.line_no,
                                  .entity_type = Kind::type,
                                  .operation = line.operation,
                                  .subject_id = Kind::subject(line),
                                  .base_version = line.base_version,
                                  .applied = line.applied,
                                  .fields = {}};
        // The row as the maker read it, so a line that has applied still shows
        // what it changed. A new row has nothing before it.
        if (line.operation == "put")
            d.fields = Kind::fields(Kind::at_version(ctx, line), line);
        out.push_back(std::move(d));
    }
}

/**
 * @brief Stamps the lines of one kind with the request and writes them.
 */
template <typename Kind>
void write_kind(const ores::database::context& ctx,
                std::vector<typename Kind::line> rows,
                const boost::uuids::uuid& request_id,
                int& line_no) {
    for (auto& row : rows) {
        ++line_no;
        row.tenant_id = ctx.tenant_id();
        row.id = ores::utility::uuid::uuid_v7_generator{}();
        row.request_id = request_id;
        row.line_no = line_no;
        row.modified_by = ctx.actor();
        row.performed_by = ctx.service_account();
        // The maker's reason and commentary are the intent the apply replays, so
        // they are kept. A line with none takes the default.
        if (row.change_reason_code.empty())
            row.change_reason_code = "system.new_record";
    }
    if (!rows.empty())
        typename Kind::repo{}.write(ctx, rows);
}

counterparty_lines stored_lines(const ores::database::context& ctx, const std::string& request_id) {
    return {.counterparties = counterparty_proposal_lines_of<counterparty_kind>(ctx, request_id),
            .identifiers = counterparty_proposal_lines_of<identifier_kind>(ctx, request_id),
            .links = counterparty_proposal_lines_of<link_kind>(ctx, request_id)};
}

void sort_parts(counterparty_preview& result) {
    std::ranges::sort(result.part_codes);
}

}

bool counterparty_preview::refused() const {
    return std::ranges::any_of(lines, [](const auto& l) { return !l.refusal.empty(); });
}

counterparty_proposal_service::counterparty_proposal_service(ores::database::context ctx)
    : ctx_(std::move(ctx)) {}

counterparty_preview counterparty_proposal_service::preview(const counterparty_lines& lines) {
    // The transaction is never committed. Its destructor rolls back every write
    // the lines made, so the live tables are left as they were and the
    // notifications the writes queued are never sent.
    db::unit_of_work uow(ctx_);
    const auto& ctx = uow.ctx();

    counterparty_preview result;
    // A request is carried out in two passes. The removals go first, the rows that
    // name another row before the row they name, so that nothing is removed while
    // something still points at it. The writes go second, the row named before the
    // rows that name it, so that nothing is written before the row it points at.
    // The line numbers do not follow this order: they are the order the maker gave.
    const int identifiers_at = static_cast<int>(lines.counterparties.size()) + 1;
    const int links_at = identifiers_at + static_cast<int>(lines.identifiers.size());
    preview_kind<link_kind>(ctx, lines.links, links_at, "delete", result);
    preview_kind<identifier_kind>(ctx, lines.identifiers, identifiers_at, "delete", result);
    preview_kind<counterparty_kind>(ctx, lines.counterparties, 1, "delete", result);
    preview_kind<counterparty_kind>(ctx, lines.counterparties, 1, "put", result);
    preview_kind<identifier_kind>(ctx, lines.identifiers, identifiers_at, "put", result);
    preview_kind<link_kind>(ctx, lines.links, links_at, "put", result);
    std::ranges::sort(result.lines, {}, &counterparty_line_outcome::line_no);
    sort_parts(result);
    return result;
}

counterparty_preview counterparty_proposal_service::recheck(const std::string& request_id) {
    const auto stored = stored_lines(ctx_, request_id);
    if (stored.size() > max_lines)
        throw std::runtime_error("The request holds more lines than a request may hold.");
    return preview(stored);
}

std::vector<counterparty_line_delta>
counterparty_proposal_service::deltas(const std::string& request_id) {
    std::vector<counterparty_line_delta> out;
    deltas_kind<counterparty_kind>(ctx_, request_id, out);
    deltas_kind<identifier_kind>(ctx_, request_id, out);
    deltas_kind<link_kind>(ctx_, request_id, out);
    std::ranges::sort(out, {}, &counterparty_line_delta::line_no);
    return out;
}

counterparty_apply counterparty_proposal_service::apply(const std::string& request_id) {
    ores::inbox::service::approval_lifecycle lifecycle(ctx_);
    const auto request = lifecycle.request(request_id);
    if (!request)
        return {.refusal = "No such request."};
    if (request->state_code != "approved") {
        return {.refusal = "The request is " + request->state_code + " and cannot be applied."};
    }

    counterparty_apply out;
    try {
        db::unit_of_work uow(ctx_);
        const auto& ctx = uow.ctx();

        // Two applies of one request must not both write. The request row is
        // locked for the transaction, and its state is read again under the lock,
        // because the read above was made before it.
        in_transaction(ctx, support::lock_request_sql(request_id));
        const auto locked = ores::inbox::service::approval_lifecycle(ctx).request(request_id);
        if (!locked || locked->state_code != "approved") {
            return {.refusal = "The request is " +
                               (locked ? locked->state_code : std::string("gone")) +
                               " and cannot be applied."};
        }

        const auto stored = stored_lines(ctx, request_id);
        if (stored.size() > max_lines) {
            out.refusal = "The request holds more lines than a request may hold.";
            out.failed_line = 1;
            throw std::length_error(out.refusal);
        }

        // The same two passes as the preview: removals child first, then writes
        // parent first.
        const bool refused =
            apply_kind<link_kind>(ctx, request_id, stored.links, "delete", out) ||
            apply_kind<identifier_kind>(ctx, request_id, stored.identifiers, "delete", out) ||
            apply_kind<counterparty_kind>(ctx, request_id, stored.counterparties, "delete", out) ||
            apply_kind<counterparty_kind>(ctx, request_id, stored.counterparties, "put", out) ||
            apply_kind<identifier_kind>(ctx, request_id, stored.identifiers, "put", out) ||
            apply_kind<link_kind>(ctx, request_id, stored.links, "put", out);
        if (!refused) {
            uow.commit();
            out.applied = true;
            return out;
        }
    } catch (const std::length_error& e) {
        // The request is over the limit: a retry reads the same lines. Only the
        // throw above raises this here: apply_kind catches logic_error, but it
        // never sees that throw.
        out.lines_applied = 0;
        out.refusal = e.what();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(counterparty_proposal_log(), error)
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
            BOOST_LOG_SEV(counterparty_proposal_log(), warn)
                << "Request " << request_id << " was refused but is " << moved.state_code
                << ", not apply_failed: " << moved.message;
        }
    } catch (const std::exception& e) {
        // Nothing was written and the request is still approved, so the apply may
        // run again and meet the same refusal.
        BOOST_LOG_SEV(counterparty_proposal_log(), error)
            << "Request " << request_id << " could not be moved to apply_failed: " << e.what();
        return {.transient = true, .refusal = e.what()};
    }
    return out;
}

counterparty_proposal counterparty_proposal_service::raise(const counterparty_lines& lines,
                                                           const std::string& reason) {
    counterparty_proposal out;
    if (lines.size() == 0) {
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

    // The preview and the raise are separate transactions, so a row can change
    // between them. The stored base version is then stale, and the apply
    // re-checks it against the live row before it writes.
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
        kind = lifecycle.kind(counterparty_proposal_kind_code);
        if (!kind)
            throw std::runtime_error(std::string("No such kind of request: ") +
                                     counterparty_proposal_kind_code);
        const auto me = lifecycle.actor_account_id();
        if (!me)
            throw std::runtime_error("The signed-in account was not found.");

        raised = lifecycle.raise(*kind, reason, *me, out.preview.part_codes);

        int line_no = 0;
        write_kind<counterparty_kind>(ctx, lines.counterparties, raised->id, line_no);
        write_kind<identifier_kind>(ctx, lines.identifiers, raised->id, line_no);
        write_kind<link_kind>(ctx, lines.links, raised->id, line_no);
        uow.commit();
    }

    // The request stands whether or not anybody is told, so the telling happens
    // after the commit and a failure there is only logged.
    ores::inbox::service::approval_lifecycle lifecycle(ctx_);
    ores::inbox::service::approval_announcer(ctx_).tell_open_parts(lifecycle, *kind, *raised);

    out.request = lifecycle.request(boost::uuids::to_string(raised->id));
    out.message = "Raised.";
    return out;
}

}
