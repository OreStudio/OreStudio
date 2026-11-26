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
#include "ores.marketdata.service/app/publication_lifecycle.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <set>
#include <stdexcept>
#include <string>

namespace ores::marketdata::service::app {

const publication_kind& publication_kind_of(derived_kind kind) {
    switch (kind) {
        case derived_kind::curve:
            return curve_publication_kind();
        case derived_kind::fx_forward:
            return fx_forward_publication_kind();
        case derived_kind::fx_vol_surface:
            return fx_vol_surface_publication_kind();
    }
    throw std::logic_error("The lifecycle is asked for a kind it does not declare.");
}

std::string raise_permission_of(const ores::database::context& ctx, std::string_view kind_code) {
    ores::inbox::service::approval_lifecycle lifecycle(ctx);
    const auto kind = lifecycle.kind(std::string(kind_code));
    if (!kind)
        throw std::runtime_error("No such kind of request: " + std::string(kind_code) + ".");
    return kind->raise_permission_code.value_or(std::string{});
}

publication_subject resolve_publication_subject(const ores::database::context& ctx,
                                                derived_kind kind,
                                                const std::string& subject_key,
                                                std::chrono::system_clock::time_point as_of) {
    const auto party = ctx.party_id();
    return publication_kind_of(kind).resolve_subject(
        ctx, party ? *party : boost::uuids::uuid{}, subject_key, as_of);
}

publication_raise raise_publication(const ores::database::context& ctx,
                                    derived_kind kind,
                                    const std::vector<publication_request>& subjects,
                                    const std::string& reason,
                                    const boost::uuids::uuid& party_id,
                                    set_raise policy) {
    if (subjects.empty())
        throw std::invalid_argument("A set of publishes names at least one subject to publish.");
    if (reason.empty())
        throw std::invalid_argument(
            "Say why the set should be published, so the decider reads a reason.");

    // A set names a subject once. The store would refuse the repeat at its
    // natural key, in words a caller cannot read, so the repeat is refused here,
    // for the whole set, with the subject named.
    std::set<std::string> named;
    for (const auto& subject : subjects)
        if (!named.insert(subject.subject_key).second)
            throw std::invalid_argument("The set names the subject " + subject.subject_key +
                                        " twice.");

    const auto& kind_act = publication_kind_of(kind);
    // The party the raise acts for is the caller's, or the session's when the
    // caller states none; a subject of another party is refused either way.
    std::optional<boost::uuids::uuid> party = ctx.party_id();
    if (!party_id.is_nil())
        party = party_id;

    // Each member is judged before anything is raised, so a decider is never
    // asked to answer for a subject the service would refuse to publish. A
    // refusal leaves the rest of the set to be raised, and a set with no
    // publishable member refuses the whole raise in the first member's words.
    publication_raise out;
    std::vector<publication_subject> accepted;
    for (const auto& subject : subjects) {
        try {
            auto resolved = kind_act.resolve_subject(
                ctx, party ? *party : boost::uuids::uuid{}, subject.subject_key, subject.as_of);
            if (const auto why = kind_act.refusal_of_raise(ctx, resolved)) {
                if (policy == set_raise::whole_set)
                    throw std::invalid_argument(*why);
                out.refused.emplace_back(subject.subject_key, *why);
                continue;
            }
            accepted.push_back(std::move(resolved));
        } catch (const std::exception& e) {
            if (policy == set_raise::whole_set)
                throw;
            out.refused.emplace_back(subject.subject_key, e.what());
        }
    }
    if (accepted.empty()) {
        // Nothing to ask for: the first refusal is the answer, in the words the
        // single-subject raise used before a set existed.
        throw std::invalid_argument(out.refused.front().second);
    }

    // The kind, the asker and the rule that may close the raise are read before
    // the transaction opens: a transaction's tenant and parties are fixed when
    // it opens, and the rule is the tenant's.
    const auto& declaration = declaration_of(kind);
    ores::inbox::service::approval_lifecycle reader(ctx);
    const auto kind_row = reader.kind(std::string(declaration.publish_kind_code));
    if (!kind_row)
        throw std::runtime_error(
            "No such kind of request: " + std::string(declaration.publish_kind_code) + ".");
    const auto me = reader.actor_account_id();
    if (!me)
        throw std::runtime_error("The signed-in account was not found.");
    const auto approval_policy = reader.policy_for(*kind_row, false);

    // The request and every subject it names are one act, so no request ever
    // names half a set.
    ores::database::repository::unit_of_work uow(ctx);
    const auto& tx = uow.ctx();
    const auto raised = ores::inbox::service::approval_lifecycle(tx).raise_under(
        approval_policy, *kind_row, reason, *me);
    for (const auto& subject : accepted)
        kind_act.write_publish(tx, raised.id, subject);
    uow.commit();

    out.request_id = boost::uuids::to_string(raised.id);
    out.state_code = raised.state_code;
    out.raised = std::move(accepted);
    return out;
}

publication_result apply_publication(const ores::database::context& ctx,
                                     derived_kind kind,
                                     const std::string& request_id,
                                     const publication_subject& attempted,
                                     const std::string& publisher) {
    const auto& kind_act = publication_kind_of(kind);

    auto verdict = kind_act.verdict_on_publish(ctx, request_id, attempted);
    publication_result out;
    out.state_code = verdict.state_code;
    if (!verdict.approved || !verdict.facts) {
        out.status = publication_status::refused;
        out.message = verdict.message;
        return out;
    }

    // A deferred build writes nothing and stamps nothing, so the request is not
    // spent on a set whose inputs have not arrived: the next attempt applies it.
    if (const auto deferral = kind_act.prepare(ctx, verdict.facts->subject)) {
        out.status = publication_status::deferred;
        out.message = *deferral;
        return out;
    }

    auto facts = *verdict.facts;
    facts.published_at = std::chrono::floor<std::chrono::seconds>(std::chrono::system_clock::now());
    facts.published_by =
        publisher.empty() ? ores::service::messaging::delegated_actor(ctx) : publisher;
    {
        // The set this one replaces ends in the same act, and both writes are one
        // transaction: a series promises one live set, so an apply that ended the
        // previous set but failed to write its successor would leave a series
        // with none, and one that wrote the successor first and then failed would
        // leave two.
        ores::database::repository::unit_of_work uow(ctx);
        const auto& tx = uow.ctx();
        facts = kind_act.publish(tx, facts);
        // The notice is a row written in the transaction that publishes the set,
        // so a publish that rolled back never told its readers of a move that did
        // not happen, and one commit carries both. Telling is still never the
        // act: a notice the announcer cannot raise is logged there.
        kind_act.announce_published(tx, facts);
        uow.commit();
    }

    out.status = publication_status::published;
    out.facts = std::move(facts);
    return out;
}

namespace {

publication_answer answer_of(const publication_result& applied, const std::string& request_id) {
    publication_answer out;
    out.request_id = request_id;
    out.state_code = applied.state_code;
    out.message = applied.message;
    out.published_by = applied.facts.published_by;
    switch (applied.status) {
        case publication_status::published:
            out.outcome = publication_outcome::published;
            out.published_at = applied.facts.published_at;
            out.message = "Published.";
            break;
        case publication_status::deferred:
            out.outcome = publication_outcome::deferred;
            break;
        case publication_status::refused:
            out.outcome = publication_outcome::refused;
            break;
    }
    return out;
}

}

publication_answer answer_publication(const ores::database::context& ctx,
                                      derived_kind kind,
                                      const std::string& subject_key,
                                      std::chrono::system_clock::time_point as_of,
                                      const std::string& request_id,
                                      const std::string& reason,
                                      const boost::uuids::uuid& party_id) {
    const auto attempted =
        publication_kind_of(kind).resolve_subject(ctx, party_id, subject_key, as_of);

    if (!request_id.empty())
        return answer_of(apply_publication(ctx, kind, request_id, attempted), request_id);

    const auto raised = raise_publication(ctx,
                                          kind,
                                          {{.subject_key = subject_key, .as_of = as_of}},
                                          reason,
                                          party_id,
                                          set_raise::keep_acceptable);
    if (raised.state_code != "approved") {
        publication_answer out;
        out.outcome = publication_outcome::raised;
        out.request_id = raised.request_id;
        out.state_code = raised.state_code;
        out.message = "Raised. The publish waits on the decision.";
        return out;
    }
    return answer_of(apply_publication(ctx, kind, raised.request_id, raised.raised.front()),
                     raised.request_id);
}

}
