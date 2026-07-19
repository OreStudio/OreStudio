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
#ifndef ORES_SYNTHETIC_API_MESSAGING_PREVIEW_IR_CURVE_SHAPE_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_PREVIEW_IR_CURVE_SHAPE_PROTOCOL_HPP

#include <string>
#include <vector>

namespace ores::synthetic::messaging {

/**
 * @brief One (possibly unsaved) Curve Template row, as edited in an IR curve editor's Curve
 * Template tab -- deliberately not the persisted ir_curve_template_entry domain struct, since a
 * row being edited has no id/party_id/tenant_id yet.
 */
struct preview_ir_curve_template_row {
    int sequence_index = 0;
    std::string start_tenor_code;
    std::string end_tenor_code;
    std::string instrument_code;
};

/**
 * @brief Request a stateless preview of the curve shape (rate per Curve Template entry) implied
 * by the given process parameters and (possibly unsaved) template rows.
 *
 * The curve-shape analogue of simulate_ir_curve_paths_request: derives each row's rate the same
 * way ir_curve_feed does (IYieldCurveProcess::discount_factor() + curve_instrument_pricer, via the
 * shared price_ir_curve_entry()), from the process's *current* state (no ticking) -- so an editor
 * can show "if I save these parameters and this template, here is the curve you'd publish" before
 * committing anything. Stateless: nothing is persisted or published.
 */
struct preview_ir_curve_shape_request {
    using response_type = struct preview_ir_curve_shape_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ir_curve.preview_shape";

    static constexpr int max_entries = 50;

    /** @brief Short-rate process engine: "vasicek", "cir", or "hull_white". */
    std::string process_type = "vasicek";
    double kappa = 0.0;
    double theta = 0.0;
    double sigma = 0.0;
    double initial_rate = 0.0;

    /** @brief References ores.refdata.payment_frequency.code -- needed to build a Swap row's
     * fixed-leg schedule. */
    std::string fixed_leg_payment_frequency_code = "Annual";

    std::vector<preview_ir_curve_template_row> entries;
};

struct preview_ir_curve_shape_point {
    int sequence_index = 0;
    std::string start_tenor_code;
    std::string end_tenor_code;
    double rate = 0.0;
};

struct preview_ir_curve_shape_response {
    bool success = false;
    std::string message;

    /** @brief One point per request entry, sorted by sequence_index. */
    std::vector<preview_ir_curve_shape_point> points;
};

}

#endif
