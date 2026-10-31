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
#include "ores.reporting.core/service/cut_resolution_service.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.reporting.core/repository/cut_kind_repository.hpp"
#include "ores.reporting.core/repository/cut_repository.hpp"
#include "ores.reporting.core/service/cut_take_plan.hpp"
#include <algorithm>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::reporting::service {

using namespace ores::logging;

namespace {

/**
 * @brief The window the resolution reads the cuts of the report's scope in.
 *
 * The year the report covers: a month end or a year end selection names an
 * instant later in the covered year than the date the report covers, so a
 * narrower window would hide the body the selection asked for.
 */
std::pair<std::chrono::system_clock::time_point, std::chrono::system_clock::time_point>
covered_year_window(const std::chrono::system_clock::time_point& covered_date) {
    const std::chrono::year_month_day covered(
        std::chrono::floor<std::chrono::days>(covered_date));
    const auto first = std::chrono::sys_days(covered.year() / std::chrono::January / 1);
    const auto after = std::chrono::sys_days((covered.year() + std::chrono::years(1)) /
                                             std::chrono::January / 1);
    return {std::chrono::system_clock::time_point(first),
            std::chrono::system_clock::time_point(after)};
}

}

cut_resolution_service::cut_resolution_service(context ctx)
    : ctx_(std::move(ctx)) {}

cut_resolution
cut_resolution_service::preview(const cut_selection& selection,
                                const cut_resolution_context& ctx) {
    return run(selection, ctx, true);
}

cut_resolution cut_resolution_service::resolve(const cut_selection& selection,
                                               const cut_resolution_context& ctx) {
    return run(selection, ctx, false);
}

cut_resolution cut_resolution_service::run(const cut_selection& selection,
                                           const cut_resolution_context& ctx,
                                           bool preview_only) {
    repository::cut_kind_repository kinds_repo;
    repository::cut_repository cuts_repo;

    auto kinds = kinds_repo.read_latest(ctx_);
    const auto [from, to] = covered_year_window(ctx.covered_date);
    auto candidates = cuts_repo.read_for_scope_in_window(
        ctx_, ctx.scope_code, ctx.business_centre_code, from, to);

    auto context = ctx;
    context.preview = preview_only;

    BOOST_LOG_SEV(lg(), debug) << "Resolving the selection of kind " << selection.kind_code
                               << " for scope " << context.scope_code << " over "
                               << candidates.size() << " candidate cuts.";

    auto resolution = resolve_cut_selection(selection, kinds, candidates, context);
    if (!resolution.mint || preview_only)
        return resolution;

    const auto kind_it =
        std::find_if(kinds.begin(), kinds.end(), [&selection](const auto& k) {
            return k.code == selection.kind_code;
        });
    if (kind_it == kinds.end())
        return resolution;

    cut_resolution resolved;
    resolved.cut = mint(*resolution.mint, *kind_it);
    return resolved;
}

domain::cut cut_resolution_service::mint(const cut_take_plan& plan,
                                         const domain::cut_kind& kind) {
    repository::cut_repository cuts_repo;

    // The occasion is the kind, the scope, the centre and the instant. Its takes
    // are read over the instant itself, because a LIVE body is separated from
    // its neighbours by the second and not by the day.
    auto same_occasion = cuts_repo.read_by_kind_and_month(
        ctx_, plan.kind_code, plan.timepoint, plan.timepoint + std::chrono::seconds(1));
    same_occasion.erase(std::remove_if(same_occasion.begin(),
                                       same_occasion.end(),
                                       [&plan](const auto& cut) {
                                           return cut.scope_code != plan.scope_code ||
                                                  cut.business_centre_code !=
                                                      plan.business_centre_code ||
                                                  cut.timepoint != plan.timepoint;
                                       }),
                        same_occasion.end());

    boost::uuids::random_generator rg;
    const auto cut = plan_take(plan, kind, rg(), next_take_ordinal(same_occasion));
    cuts_repo.write(ctx_, cut);

    BOOST_LOG_SEV(lg(), info) << "Minted cut " << boost::uuids::to_string(cut.id) << " of kind "
                              << cut.kind_code << " for scope " << cut.scope_code << ".";
    return cut;
}

}
