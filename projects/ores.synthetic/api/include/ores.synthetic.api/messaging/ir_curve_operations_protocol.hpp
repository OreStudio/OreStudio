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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_SYNTHETIC_API_MESSAGING_IR_CURVE_OPERATIONS_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_IR_CURVE_OPERATIONS_PROTOCOL_HPP

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace ores::synthetic::messaging {

/**
 * @brief One named process parameter, as edited in an IR curve editor's
 * parameter table.
 *
 * The wire-shaped analogue of a {parameter_definition_id, value} row: the
 * row-based store joins values onto the system-tenant parameter definitions
 * catalogue, but a stateless preview request carries the name itself, because
 * the screen already has the definitions on screen, and lets the handler
 * re-materialise the catalogue entry on the fly.
 */
struct parameter_spec {
    std::string parameter_name;
    double parameter_value = 0.0;
};

/**
 * @brief Request a batch (dry-run) simulation of short-rate sample paths.
 *
 * The IR curve analogue of simulate_fx_spot_paths_request. Stateless: the
 * caller names the process parameters rather than a stored config, so an
 * unsaved editor can preview what it is about to save.
 */
struct simulate_ir_curve_paths_request {
    using response_type = struct simulate_ir_curve_paths_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.simulate_ir_curve_paths";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief Short-rate process engine: "vasicek", "cox_ingersoll_ross",
     * "hull_white", or "two_factor_gaussian".
     */
    std::string process_type = "vasicek";
    /** @brief The process's named parameters, one entry per parameter. */
    std::vector<parameter_spec> parameters;
    /** @brief Number of ticks (steps) to generate per path. */
    int num_ticks = 100;
    /** @brief Number of independent sample paths to generate (for contrast). */
    int num_paths = 5;
    /** @brief Base RNG seed; path i uses seed + i. Change to reseed. */
    std::uint32_t seed = 1;
};

/**
 * @brief The generated batch, and why it is empty when it is.
 */
struct simulate_ir_curve_paths_response {
    bool success = false;
    std::string message;
    /** @brief num_paths series, each of num_ticks short-rate values. */
    std::vector<std::vector<double>> paths;
};

/**
 * @brief One (possibly unsaved) Curve Template row, as edited in an IR curve
 * editor's Curve Template tab.
 *
 * Deliberately not the persisted ir_curve_template_entry domain struct: a row
 * being edited has no id, party_id or tenant_id yet.
 */
struct preview_ir_curve_template_row {
    int sequence_index = 0;
    std::string start_tenor_code;
    std::string end_tenor_code;
    std::string instrument_code;
};

/**
 * @brief Request a stateless preview of the curve shape (rate per Curve
 * Template entry) implied by the given process parameters and template rows.
 *
 * Derives each row's rate the same way ir_curve_feed does
 * (IYieldCurveProcess::discount_factor() + curve_instrument_pricer, via the
 * shared price_ir_curve_entry()), from the process's current state, with no
 * ticking, so an editor can show "if I save these parameters and this
 * template, here is the curve you would publish" before committing anything.
 * Nothing is persisted or published.
 */
struct preview_ir_curve_shape_request {
    using response_type = struct preview_ir_curve_shape_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.preview_ir_curve_shape";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief Short-rate process engine: "vasicek", "cox_ingersoll_ross",
     * "hull_white", or "two_factor_gaussian".
     */
    std::string process_type = "vasicek";
    /** @brief The process's named parameters, one entry per parameter. */
    std::vector<parameter_spec> parameters;
    /**
     * @brief References ores.refdata.payment_frequency.code; needed to build a
     * Swap row's fixed-leg schedule.
     */
    std::string fixed_leg_payment_frequency_code = "Annual";
    /** @brief The template rows to price, in the order the editor shows them. */
    std::vector<preview_ir_curve_template_row> entries;
};

/**
 * @brief One priced Curve Template row.
 */
struct preview_ir_curve_shape_point {
    int sequence_index = 0;
    std::string start_tenor_code;
    std::string end_tenor_code;
    double rate = 0.0;
};

/**
 * @brief The priced curve, and why it is empty when it is.
 */
struct preview_ir_curve_shape_response {
    bool success = false;
    std::string message;
    /** @brief One point per request entry, sorted by sequence_index. */
    std::vector<preview_ir_curve_shape_point> points;
};

}

#endif
