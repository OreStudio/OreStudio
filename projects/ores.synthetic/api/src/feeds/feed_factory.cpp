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
#include "ores.synthetic.api/feeds/feed_factory.hpp"
#include <stdexcept>

namespace ores::synthetic::feed {

void feed_factory::register_kind(std::string kind, builder b) {
    builders_[std::move(kind)] = std::move(b);
}

std::shared_ptr<ores::marketdata::domain::IFeed> feed_factory::make(
    const std::string& kind, const feed_build_context& ctx, const feed_build_input& input) const {
    const auto it = builders_.find(kind);
    if (it == builders_.end())
        throw std::invalid_argument("feed_factory: no builder registered for kind '" + kind + "'");
    try {
        return it->second(ctx, input);
    } catch (const std::bad_variant_access& e) {
        throw std::invalid_argument("feed_factory: input is not the build input for kind '" + kind +
                                    "': " + e.what());
    }
}

std::vector<std::string> feed_factory::kinds() const {
    std::vector<std::string> out;
    out.reserve(builders_.size());
    for (const auto& [kind, _] : builders_)
        out.push_back(kind);
    return out;
}

namespace {

std::shared_ptr<ores::marketdata::domain::IFeed> build_fx_spot(const feed_build_context& ctx,
                                                               const feed_build_input& input) {
    const auto& in = std::get<fx_spot_feed_build_input>(input);
    return make_fx_spot_feed(ctx.nats, in.config, in.components, in.binding_mode);
}

std::shared_ptr<ores::marketdata::domain::IFeed> build_ir_curve(const feed_build_context& ctx,
                                                                const feed_build_input& input) {
    const auto& in = std::get<ir_curve_feed_build_input>(input);
    return make_ir_curve_feed(ctx.nats,
                              ctx.auth_nats,
                              in.config,
                              in.entries,
                              in.values,
                              in.definitions,
                              in.refctx,
                              ctx.caller_bearer_token);
}

} // namespace

feed_factory make_default_feed_factory() {
    feed_factory factory;
    factory.register_kind(std::string(fx_spot_feed_kind), build_fx_spot);
    factory.register_kind(std::string(ir_curve_feed_kind), build_ir_curve);
    return factory;
}

}
