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
#include "ores.ore.core/domain/credit_simulation_projection.hpp"
#include "ores.ore.core/domain/credit_simulation_grid.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include "ores.ore.core/xml/emitter.hpp"
#include "ores.ore.core/xml/projection.hpp"
#include "ores.platform/numeric/floating_point.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <charconv>
#include <cstddef>
#include <format>
#include <sstream>
#include <stdexcept>
#include <string_view>
#include <unordered_map>

namespace ores::ore::domain {

using namespace ores::logging;

namespace {

namespace xg = xml::generated::credit_simulation;

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

double parse_double(const std::string& text) {
    // platform::numeric, not std::from_chars: libc++ ships the floating-point
    // from_chars as a deleted function, so the standard call compiles on Linux
    // and MSVC and stops the macOS build.
    const auto parsed = ores::platform::numeric::parse_double(text);
    if (!parsed.has_value())
        throw std::runtime_error("A transition matrix bound is not a number: " + text);
    return *parsed;
}

std::string format_double(double value) {
    char buffer[64];
    const auto written = std::to_chars(buffer, buffer + sizeof(buffer), value);
    if (written.ec != std::errc{})
        throw std::runtime_error("Could not format a transition matrix bound");
    return std::string(buffer, written.ptr);
}

/**
 * @brief Whether a flag the schema spells as an enumeration is on.
 *
 * ORE writes a true flag six ways. The model holds the truth, not the token,
 * so every spelling reads the same and an export writes Y or N.
 */
auto is_on(const std::string_view spelling) -> bool {
    return spelling == "Y" || spelling == "YES" || spelling == "TRUE" || spelling == "True" ||
           spelling == "true" || spelling == "1";
}

/**
 * @brief States a flag, or refuses a table that has no such column.
 */
void set_token(const xml::record_type& type,
               void* record,
               const std::string_view name,
               const std::string& token) {
    if (!xml::set_enumeration(type, record, name, token))
        throw std::runtime_error(
            std::format("'{}' is not a flag spelling for {} in {}", token, name, type.name));
}

void set_flag(const xml::record_type& type, void* record, const std::string_view name, bool on) {
    if (!xml::set_enumeration(type, record, name, on ? "Y" : "N"))
        throw std::runtime_error(std::format("the {} table has no flag '{}'", type.name, name));
}

}

credit_simulation_projection::document credit_simulation_projection::read(std::string_view text,
                                                                          std::string_view origin) {
    document record;
    const auto bound = xml::binder::bind(xg::credit_simulation_document(), text, &record);
    if (!bound) {
        const auto where = origin.empty() ? std::string("credit simulation") : std::string(origin);
        throw std::invalid_argument(
            std::format("{}: line {}: {}", where, bound.error().line, bound.error().message));
    }
    return record;
}

std::string credit_simulation_projection::write(const document& v) {
    std::string out;
    const auto emitted = xml::emitter::emit(xg::credit_simulation_document(), &v, out);
    if (!emitted)
        throw std::runtime_error(emitted.error().message);
    return out;
}

mapped_credit_simulation credit_simulation_projection::map(const document& v) {
    mapped_credit_simulation mapped;

    auto& config = mapped.config;
    config.id = new_uuid();
    config.name = "CreditSimulation";
    const auto& risk_type = xml::projection::child(*xg::credit_simulation_document().root, "Risk");
    const auto token = [&risk_type, &v](const std::string_view name) -> std::string {
        return xml::enumeration_spelling(risk_type, &v.risk, name).value_or("");
    };
    const auto flag = [&token](const std::string_view name) {
        return is_on(token(name));
    };
    config.market = token("Market");
    config.credit = token("Credit");
    config.zero_market_pnl = flag("ZeroMarketPnl");
    config.double_default = flag("DoubleDefault");
    config.evaluation = v.risk.evaluation;
    config.seed = static_cast<int>(v.risk.seed);
    config.paths = static_cast<int>(v.risk.paths);
    config.credit_mode = v.risk.credit_mode;
    config.loan_exposure_mode = v.risk.loan_exposure_mode;
    set_audit(config);

    std::unordered_map<std::string, boost::uuids::uuid> matrix_ids;
    for (const auto& tm : v.transition_matrices.transition_matrix) {
        analytics::domain::credit_simulation_matrix_config matrix;
        matrix.id = new_uuid();
        matrix.name = tm.name;
        matrix.t0 = tm.data.t0.empty() ? 0.0 : parse_double(tm.data.t0);
        matrix.t1 = tm.data.t1.empty() ? 0.0 : parse_double(tm.data.t1);
        set_audit(matrix);
        matrix_ids[matrix.name] = matrix.id;

        const auto grid = parse_credit_simulation_grid(tm.data.value);
        const auto side = grid.side();
        if (side != rating_count)
            throw std::runtime_error("A transition matrix is not eight by eight, so it has no "
                                     "rating rows: " +
                                     tm.name);

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

    for (const auto& e : v.entities.entity) {
        analytics::domain::credit_simulation_entity_config entity;
        entity.id = new_uuid();
        entity.credit_simulation_config_id = config.id;
        entity.name = e.name;
        const auto found = matrix_ids.find(e.transition_matrix);
        if (found == matrix_ids.end())
            throw std::runtime_error("An entity names a transition matrix that does not exist: " +
                                     e.transition_matrix);
        entity.transition_matrix_id = found->second;
        entity.initial_state = static_cast<int>(e.initial_state);
        entity.factor_loadings = e.factor_loadings;
        set_audit(entity);
        mapped.entities.push_back(std::move(entity));
    }

    // ORE writes the netting sets as a comma separated list in one element. A
    // list is rows, so each code becomes a row and the position keeps the order.
    const std::string codes(v.netting_set_ids);
    std::string current;
    std::istringstream code_stream(codes);
    int code_position = 0;
    while (std::getline(code_stream, current, ',')) {
        const auto first = current.find_first_not_of(" \t\r\n");
        if (first == std::string::npos)
            continue;
        const auto last = current.find_last_not_of(" \t\r\n");
        analytics::domain::credit_simulation_netting_set_config netting_set;
        netting_set.id = new_uuid();
        netting_set.credit_simulation_config_id = config.id;
        netting_set.netting_set_id = current.substr(first, last - first + 1);
        netting_set.position = code_position++;
        set_audit(netting_set);
        mapped.netting_sets.push_back(std::move(netting_set));
    }
    return mapped;
}

credit_simulation_projection::document
credit_simulation_projection::reverse(const mapped_credit_simulation& v) {
    const auto& root = xg::credit_simulation_document().root;
    const auto& matrices_type = xml::projection::child(*root, "TransitionMatrices");
    const auto& matrix_type = xml::projection::child(matrices_type, "TransitionMatrix");
    const auto& data_type = xml::projection::child(matrix_type, "Data");
    const auto& entities_type = xml::projection::child(*root, "Entities");
    const auto& entity_type = xml::projection::child(entities_type, "Entity");
    const auto& risk_type = xml::projection::child(*root, "Risk");

    document record;

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
            throw std::runtime_error("A transition matrix needs eight rating rows: " + matrix.name);
        credit_simulation_grid grid;
        grid.values.assign(side * side, 0.0);
        for (std::size_t r = 0; r < side; ++r) {
            const double probabilities[rating_count] = {rows[r].p_aaa,
                                                        rows[r].p_aa,
                                                        rows[r].p_a,
                                                        rows[r].p_baa,
                                                        rows[r].p_ba,
                                                        rows[r].p_b,
                                                        rows[r].p_c,
                                                        rows[r].p_default};
            for (std::size_t c = 0; c < rating_count; ++c)
                grid.values[r * side + c] = probabilities[c];
        }

        xg::transitionmatrix tm;
        tm.name = matrix.name;
        xml::projection::state(matrix_type, &tm, "Name");
        tm.data.value = format_credit_simulation_grid(grid);
        tm.data.t0 = format_double(matrix.t0);
        tm.data.t1 = format_double(matrix.t1);
        xml::mark_text_present(data_type, &tm.data);
        xml::projection::state(data_type, &tm.data, "t0");
        xml::projection::state(data_type, &tm.data, "t1");
        xml::projection::state(matrix_type, &tm, "Data");

        record.transition_matrices.transition_matrix.push_back(std::move(tm));
        xml::projection::state(matrices_type, &record.transition_matrices, "TransitionMatrix");
    }

    for (const auto& entity : v.entities) {
        xg::entity e;
        e.name = entity.name;
        e.factor_loadings = entity.factor_loadings;
        const auto matrix = matrix_names.find(boost::uuids::to_string(entity.transition_matrix_id));
        if (matrix == matrix_names.end())
            throw std::runtime_error("An entity points at a transition matrix that is not mapped");
        e.transition_matrix = matrix->second;
        e.initial_state = entity.initial_state;
        xml::projection::state(entity_type, &e, "Name");
        xml::projection::state(entity_type, &e, "FactorLoadings");
        xml::projection::state(entity_type, &e, "TransitionMatrix");
        xml::projection::state(entity_type, &e, "InitialState");
        record.entities.entity.push_back(std::move(e));
        xml::projection::state(entities_type, &record.entities, "Entity");
    }

    {
        std::string joined;
        for (const auto& netting_set : v.netting_sets) {
            if (!joined.empty())
                joined += ",";
            joined += netting_set.netting_set_id;
        }
        record.netting_set_ids = joined;
        xml::projection::state(*root, &record, "NettingSetIds");
    }

    // Two flags keep the token the document used; they are held as text and
    // written back as text, so a document that said YES is answered with YES.
    set_token(risk_type, &record.risk, "Market", v.config.market);
    set_token(risk_type, &record.risk, "Credit", v.config.credit);
    set_flag(risk_type, &record.risk, "ZeroMarketPnl", v.config.zero_market_pnl);
    set_flag(risk_type, &record.risk, "DoubleDefault", v.config.double_default);
    record.risk.evaluation = v.config.evaluation;
    xml::projection::state(risk_type, &record.risk, "Evaluation");
    record.risk.seed = v.config.seed;
    xml::projection::state(risk_type, &record.risk, "Seed");
    record.risk.paths = v.config.paths;
    xml::projection::state(risk_type, &record.risk, "Paths");
    record.risk.credit_mode = v.config.credit_mode;
    xml::projection::state(risk_type, &record.risk, "CreditMode");
    record.risk.loan_exposure_mode = v.config.loan_exposure_mode;
    xml::projection::state(risk_type, &record.risk, "LoanExposureMode");
    xml::projection::state(*root, &record, "Risk");

    if (!record.transition_matrices.transition_matrix.empty())
        xml::projection::state(*root, &record, "TransitionMatrices");
    if (!record.entities.entity.empty())
        xml::projection::state(*root, &record, "Entities");

    return record;
}

}
