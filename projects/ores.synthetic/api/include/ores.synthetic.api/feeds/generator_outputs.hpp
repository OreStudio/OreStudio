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
#ifndef ORES_SYNTHETIC_API_FEEDS_GENERATOR_OUTPUTS_HPP
#define ORES_SYNTHETIC_API_FEEDS_GENERATOR_OUTPUTS_HPP

#include "ores.marketdata.core/oresmd/pillar_quote.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config.hpp"
#include "ores.synthetic.api/domain/ir_curve_template_entry.hpp"
#include "ores.synthetic.api/export.hpp"
#include <array>
#include <expected>
#include <span>
#include <string>
#include <string_view>
#include <vector>

/*
 * What each generator kind publishes, as the addresses a demand names.
 *
 * A demand names a quote by its oresmd URI, such as a pillar's swap rate at its term, a
 * spot quote, or a point of a surface. A pillar's address is the datum the feed publishes,
 * built by pillar_quote_datum() as the feed builds it, so a reader of the configuration
 * names the same address before any feed runs.
 */
namespace ores::synthetic::feed {

/// The deltas, besides ATM, a surface quotes a risk reversal and a butterfly at.
inline constexpr std::array<double, 2> fx_vol_surface_deltas{0.25, 0.10};

/**
 * @brief The address of a pillar's quote.
 *
 * @return The address, or why the pillar names no datum.
 */
ORES_SYNTHETIC_API_EXPORT std::expected<std::string, std::string>
ir_pillar_quote_uri(const ores::marketdata::core::pillar_quote& pillar);

/**
 * @brief What a curve publishes on every day: the quotes of its pillars that need no
 * resolved date, and the fixings of its own index when it states them.
 *
 * A meeting-dated pillar names a quote only on the day its dates resolve, so it is left
 * out; such a curve still names its other pillars.
 *
 * @param entries The curve's template entries. Entries of other curves are ignored.
 * @return The addresses, or why a pillar names none.
 */
ORES_SYNTHETIC_API_EXPORT std::expected<std::vector<std::string>, std::string>
ir_curve_outputs(const domain::ir_curve_generation_config& cfg,
                 const std::vector<domain::ir_curve_template_entry>& entries);

/**
 * @brief A spot quote's address in its canonical spelling.
 *
 * @return The address, or why @p quote_uri is not a spot quote.
 */
ORES_SYNTHETIC_API_EXPORT std::expected<std::string, std::string>
fx_spot_quote_uri(const std::string& quote_uri);

/**
 * @brief The address of one point of a surface: an expiry at a strike label such as ATM or
 * 25RR.
 */
ORES_SYNTHETIC_API_EXPORT std::string fx_vol_point_uri(std::string_view base,
                                                       std::string_view quote,
                                                       std::string_view expiry,
                                                       std::string_view label);

/**
 * @brief The addresses of a surface: each expiry at ATM, then at the risk reversal and the
 * butterfly of each delta.
 */
ORES_SYNTHETIC_API_EXPORT std::vector<std::string>
fx_vol_surface_outputs(std::string_view base,
                       std::string_view quote,
                       const std::vector<std::string>& expiries,
                       std::span<const double> deltas = fx_vol_surface_deltas);

}

#endif
