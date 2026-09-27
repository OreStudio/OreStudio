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
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <algorithm>
#include <cctype>
#include <charconv>
#include <cmath>
#include <cstddef>
#include <stdexcept>
#include <unordered_map>

namespace ores::ore::domain {

using namespace ores::logging;

namespace {

constexpr std::string_view audit_modified_by = "ores";
constexpr std::string_view audit_reason_code = "system.external_data_import";
constexpr std::string_view audit_commentary = "Imported from ORE XML";

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

bool parse_bool_string(const std::string& v) {
    return v == "Y" || v == "YES" || v == "TRUE" || v == "True" || v == "true" || v == "1";
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

std::string credit_simulation_mapper::format_number(double value) {
    char buffer[64];
    const auto written = std::to_chars(buffer, buffer + sizeof(buffer), value);
    if (written.ec != std::errc{})
        throw std::runtime_error("Could not format a transition matrix probability");
    return std::string(buffer, written.ptr);
}

std::vector<std::vector<double>>
credit_simulation_mapper::parse_grid(const std::string& text) {
    std::vector<double> values;
    const char* pos = text.data();
    const char* const end = pos + text.size();
    while (pos != end) {
        while (pos != end && (std::isspace(static_cast<unsigned char>(*pos)) || *pos == ','))
            ++pos;
        if (pos == end)
            break;
        const char* const token = pos;
        while (pos != end && !std::isspace(static_cast<unsigned char>(*pos)) && *pos != ',')
            ++pos;
        double value = 0.0;
        const auto parsed = std::from_chars(token, pos, value);
        if (parsed.ec == std::errc{} && parsed.ptr == pos)
            values.push_back(value);
    }

    const auto size = values.size();
    const auto dimension = static_cast<std::size_t>(
        std::llround(std::sqrt(static_cast<double>(size))));
    if (dimension == 0 || dimension * dimension != size)
        throw std::runtime_error("A transition matrix Data grid is not square");

    std::vector<std::vector<double>> grid(dimension, std::vector<double>(dimension, 0.0));
    for (std::size_t i = 0; i < dimension; ++i)
        for (std::size_t j = 0; j < dimension; ++j)
            grid[i][j] = values[i * dimension + j];
    return grid;
}

std::string credit_simulation_mapper::format_grid(const std::vector<std::vector<double>>& grid) {
    std::string text;
    for (std::size_t i = 0; i < grid.size(); ++i) {
        if (i != 0)
            text += '\n';
        for (std::size_t j = 0; j < grid[i].size(); ++j) {
            if (j != 0)
                text += ", ";
            text += format_number(grid[i][j]);
        }
    }
    return text;
}

std::vector<std::vector<double>> credit_simulation_mapper::assemble_grid(
    const std::vector<analytics::domain::credit_simulation_matrix_cell_config>& cells) {
    std::size_t dimension = 0;
    for (const auto& cell : cells) {
        const auto from = static_cast<std::size_t>(std::max(cell.from_state, 0));
        const auto to = static_cast<std::size_t>(std::max(cell.to_state, 0));
        dimension = std::max(dimension, std::max(from, to) + 1);
    }

    std::vector<std::vector<double>> grid(dimension, std::vector<double>(dimension, 0.0));
    for (const auto& cell : cells) {
        if (cell.from_state < 0 || cell.to_state < 0)
            throw std::runtime_error("A transition matrix cell carries a negative state");
        grid[static_cast<std::size_t>(cell.from_state)][static_cast<std::size_t>(cell.to_state)] =
            cell.probability;
    }
    return grid;
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
        set_audit(matrix);
        matrix_ids[matrix.name] = matrix.id;

        mapped_matrix_states states;
        states.matrix_id = matrix.id;
        if (tm.Data.t0)
            states.t0 = *tm.Data.t0;
        if (tm.Data.t1)
            states.t1 = *tm.Data.t1;

        const auto grid = parse_grid(tm.Data);
        for (std::size_t i = 0; i < grid.size(); ++i) {
            for (std::size_t j = 0; j < grid[i].size(); ++j) {
                analytics::domain::credit_simulation_matrix_cell_config cell;
                cell.id = new_uuid();
                cell.transition_matrix_id = matrix.id;
                cell.from_state = static_cast<int>(i);
                cell.to_state = static_cast<int>(j);
                cell.probability = grid[i][j];
                set_audit(cell);
                mapped.cells.push_back(std::move(cell));
            }
        }

        mapped.matrices.push_back(std::move(matrix));
        mapped.matrix_states.push_back(std::move(states));
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

    std::unordered_map<std::string, mapped_matrix_states> states_by_matrix;
    for (const auto& states : v.matrix_states)
        states_by_matrix[boost::uuids::to_string(states.matrix_id)] = states;

    std::unordered_map<std::string, std::string> matrix_names;
    for (const auto& matrix : v.matrices) {
        matrix_names[boost::uuids::to_string(matrix.id)] = matrix.name;

        domain::transitionmatrix tm;
        tm.Name = matrix.name;

        std::vector<analytics::domain::credit_simulation_matrix_cell_config> cells;
        for (const auto& cell : v.cells) {
            if (cell.transition_matrix_id == matrix.id)
                cells.push_back(cell);
        }
        std::sort(cells.begin(), cells.end(), [](const auto& lhs, const auto& rhs) {
            if (lhs.from_state != rhs.from_state)
                return lhs.from_state < rhs.from_state;
            return lhs.to_state < rhs.to_state;
        });
        tm.Data = format_grid(assemble_grid(cells));

        const auto states = states_by_matrix.find(boost::uuids::to_string(matrix.id));
        if (states != states_by_matrix.end()) {
            if (!states->second.t0.empty())
                tm.Data.t0 = states->second.t0;
            if (!states->second.t1.empty())
                tm.Data.t1 = states->second.t1;
        }

        document.TransitionMatrices.TransitionMatrix.push_back(std::move(tm));
    }

    for (const auto& entity : v.entities) {
        domain::entity e;
        e.Name = entity.name;
        e.FactorLoadings = entity.factor_loadings;
        const auto matrix = matrix_names.find(boost::uuids::to_string(entity.transition_matrix_id));
        if (matrix == matrix_names.end())
            throw std::runtime_error("An entity points at a transition matrix that is not mapped");
        e.TransitionMatrix = matrix->second;
        e.InitialState = entity.initial_state;
        document.Entities.Entity.push_back(std::move(e));
    }

    document.NettingSetIds = v.netting_set_ids;

    document.Risk.Market = make_bool(parse_bool_string(v.config.market));
    document.Risk.Credit = make_bool(parse_bool_string(v.config.credit));
    document.Risk.ZeroMarketPnl = make_bool(v.config.zero_market_pnl);
    document.Risk.Evaluation = v.config.evaluation;
    document.Risk.DoubleDefault = make_bool(v.config.double_default);
    document.Risk.Seed = v.config.seed;
    document.Risk.Paths = v.config.paths;
    document.Risk.CreditMode = v.config.credit_mode;
    document.Risk.LoanExposureMode = v.config.loan_exposure_mode;

    return document;
}

}
