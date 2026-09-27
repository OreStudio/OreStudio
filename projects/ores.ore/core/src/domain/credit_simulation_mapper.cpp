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
#include "ores.ore.core/domain/credit_simulation_mapper.hpp"
#include "ores.ore.core/domain/credit_simulation_grid.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <charconv>
#include <cstddef>
#include <stdexcept>
#include <unordered_map>

namespace ores::ore::domain {

using namespace ores::logging;

namespace {

constexpr std::string_view audit_modified_by = "ores";
constexpr std::string_view audit_reason_code = "system.external_data_import";
constexpr std::string_view audit_commentary = "Imported from ORE XML";
constexpr std::size_t rating_count = 8;

boost::uuids::uuid new_uuid() {
    static thread_local ores::utility::uuid::uuid_v7_generator generator;
    return generator();
}

template <typename T>
void set_audit(T& r) {
    r.modified_by = std::string(audit_modified_by);
    r.performed_by = std::string(audit_modified_by);
    r.change_reason_code = std::string(audit_reason_code);
    r.change_commentary = std::string(audit_commentary);
}

// Every generated text field is a distinct struct derived from xsd::string, so
// the base subobject is the only assignment target a std::string converts to.
template <typename T>
void assign_text(T& target, const std::string& value) {
    static_cast<xsd::string&>(target) = value;
}

bool parse_bool_string(const std::string& v) {
    return v == "Y" || v == "YES" || v == "TRUE" || v == "True" || v == "true" || v == "1";
}

double parse_double(const std::string& text) {
    double value = 0.0;
    const auto parsed =
        std::from_chars(text.data(), text.data() + text.size(), value);
    if (parsed.ec != std::errc{} || parsed.ptr != text.data() + text.size())
        throw std::runtime_error("A transition matrix bound is not a number: " + text);
    return value;
}

std::string format_double(double value) {
    char buffer[64];
    const auto written = std::to_chars(buffer, buffer + sizeof(buffer), value);
    if (written.ec != std::errc{})
        throw std::runtime_error("Could not format a transition matrix bound");
    return std::string(buffer, written.ptr);
}

}

bool credit_simulation_mapper::parse_bool(domain::bool_ v) {
    using b = domain::bool_;
    switch (v) {
        case b::Y:
        case b::YES:
        case b::TRUE_:
        case b::True:
        case b::true_:
        case b::_1:
            return true;
        default:
            return false;
    }
}

domain::bool_ credit_simulation_mapper::make_bool(bool v) {
    return v ? domain::bool_::Y : domain::bool_::N;
}

mapped_credit_simulation credit_simulation_mapper::map(const creditsimulation& v) {
    mapped_credit_simulation mapped;

    auto& config = mapped.config;
    config.id = new_uuid();
    config.name = "CreditSimulation";
    config.market = to_string(v.Risk.Market);
    config.credit = to_string(v.Risk.Credit);
    config.zero_market_pnl = parse_bool(v.Risk.ZeroMarketPnl);
    config.evaluation = v.Risk.Evaluation;
    config.double_default = parse_bool(v.Risk.DoubleDefault);
    config.seed = static_cast<int>(v.Risk.Seed);
    config.paths = static_cast<int>(v.Risk.Paths);
    config.credit_mode = v.Risk.CreditMode;
    config.loan_exposure_mode = v.Risk.LoanExposureMode;
    set_audit(config);

    std::unordered_map<std::string, boost::uuids::uuid> matrix_ids;
    for (const auto& tm : v.TransitionMatrices.TransitionMatrix) {
        analytics::domain::credit_simulation_matrix_config matrix;
        matrix.id = new_uuid();
        matrix.name = tm.Name;
        matrix.t0 = tm.Data.t0 ? parse_double(*tm.Data.t0) : 0.0;
        matrix.t1 = tm.Data.t1 ? parse_double(*tm.Data.t1) : 0.0;
        set_audit(matrix);
        matrix_ids[matrix.name] = matrix.id;

        const auto grid = parse_credit_simulation_grid(tm.Data);
        const auto side = grid.side();
        if (side != rating_count)
            throw std::runtime_error("A transition matrix is not eight by eight, so it has no "
                                     "rating rows: " +
                                     std::string(tm.Name));

        for (std::size_t r = 0; r < side; ++r) {
            analytics::domain::credit_simulation_matrix_row_config row;
            row.id = new_uuid();
            row.transition_matrix_id = matrix.id;
            row.from_rating = std::string(credit_rating_scale[r]);
            row.p_aaa = grid.values[r * side + 0];
            row.p_aa = grid.values[r * side + 1];
            row.p_a = grid.values[r * side + 2];
            row.p_baa = grid.values[r * side + 3];
            row.p_ba = grid.values[r * side + 4];
            row.p_b = grid.values[r * side + 5];
            row.p_c = grid.values[r * side + 6];
            row.p_default = grid.values[r * side + 7];
            set_audit(row);
            mapped.rows.push_back(std::move(row));
        }

        mapped.matrices.push_back(std::move(matrix));
    }

    for (const auto& e : v.Entities.Entity) {
        analytics::domain::credit_simulation_entity_config entity;
        entity.id = new_uuid();
        entity.credit_simulation_config_id = config.id;
        entity.name = e.Name;
        const auto found = matrix_ids.find(std::string(e.TransitionMatrix));
        if (found == matrix_ids.end())
            throw std::runtime_error("An entity names a transition matrix that does not exist: " +
                                     std::string(e.TransitionMatrix));
        entity.transition_matrix_id = found->second;
        entity.initial_state = static_cast<int>(e.InitialState);
        entity.factor_loadings = e.FactorLoadings;
        set_audit(entity);
        mapped.entities.push_back(std::move(entity));
    }

    mapped.netting_set_ids = v.NettingSetIds;
    return mapped;
}

creditsimulation credit_simulation_mapper::reverse(const mapped_credit_simulation& v) {
    creditsimulation document;

    std::unordered_map<std::string, std::string> matrix_names;
    for (const auto& matrix : v.matrices) {
        matrix_names[boost::uuids::to_string(matrix.id)] = matrix.name;

        std::vector<analytics::domain::credit_simulation_matrix_row_config> rows;
        for (const auto& row : v.rows) {
            if (row.transition_matrix_id == matrix.id)
                rows.push_back(row);
        }

        const std::size_t side = rows.size();
        if (side != rating_count)
            throw std::runtime_error("A transition matrix needs eight rating rows: " +
                                     matrix.name);
        credit_simulation_grid grid;
        grid.values.assign(side * side, 0.0);
        for (std::size_t r = 0; r < side; ++r) {
            const double probabilities[rating_count] = {
                rows[r].p_aaa, rows[r].p_aa,  rows[r].p_a, rows[r].p_baa,
                rows[r].p_ba,  rows[r].p_b,   rows[r].p_c, rows[r].p_default};
            for (std::size_t c = 0; c < rating_count; ++c)
                grid.values[r * side + c] = probabilities[c];
        }

        domain::transitionmatrix tm;
        assign_text(tm.Name, matrix.name);
        assign_text(tm.Data, format_credit_simulation_grid(grid));
        tm.Data.t0 = format_double(matrix.t0);
        tm.Data.t1 = format_double(matrix.t1);

        document.TransitionMatrices.TransitionMatrix.push_back(std::move(tm));
    }

    for (const auto& entity : v.entities) {
        domain::entity e;
        assign_text(e.Name, entity.name);
        assign_text(e.FactorLoadings, entity.factor_loadings);
        const auto matrix = matrix_names.find(boost::uuids::to_string(entity.transition_matrix_id));
        if (matrix == matrix_names.end())
            throw std::runtime_error("An entity points at a transition matrix that is not mapped");
        assign_text(e.TransitionMatrix, matrix->second);
        e.InitialState = entity.initial_state;
        document.Entities.Entity.push_back(std::move(e));
    }

    assign_text(document.NettingSetIds, v.netting_set_ids);

    document.Risk.Market = make_bool(parse_bool_string(v.config.market));
    document.Risk.Credit = make_bool(parse_bool_string(v.config.credit));
    document.Risk.ZeroMarketPnl = make_bool(v.config.zero_market_pnl);
    assign_text(document.Risk.Evaluation, v.config.evaluation);
    document.Risk.DoubleDefault = make_bool(v.config.double_default);
    document.Risk.Seed = v.config.seed;
    document.Risk.Paths = v.config.paths;
    assign_text(document.Risk.CreditMode, v.config.credit_mode);
    assign_text(document.Risk.LoanExposureMode, v.config.loan_exposure_mode);

    return document;
}

}
