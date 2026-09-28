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

#include "ores.ore.core/domain/credit_simulation_grid.hpp"
#include "ores.ore.core/domain/credit_simulation_mapper.hpp"
#include <cstddef>
#include <sstream>
#include <string>
#include <vector>

namespace ores::ore::domain {

namespace {


struct mismatch {
    bool equal = true;
    std::string message;
};

std::string describe(const std::string& path, const std::string& detail) {
    return path + ": " + detail;
}

bool same_bound(const xsd::optional<xsd::string>& lhs, const xsd::optional<xsd::string>& rhs) {
    const bool lhs_has = static_cast<bool>(lhs);
    const bool rhs_has = static_cast<bool>(rhs);
    if (lhs_has != rhs_has)
        return false;
    if (!lhs_has)
        return true;
    return std::stod(*lhs) == std::stod(*rhs);
}

mismatch compare_matrices(const creditsimulation& original,
                          const creditsimulation& exported,
                          const std::string& path) {
    const auto& lhs = original.TransitionMatrices.TransitionMatrix;
    const auto& rhs = exported.TransitionMatrices.TransitionMatrix;
    if (lhs.size() != rhs.size())
        return {false,
                describe(path,
                         "transition matrix count differs: original " + std::to_string(lhs.size()) +
                             ", exported " + std::to_string(rhs.size()))};

    for (std::size_t i = 0; i < lhs.size(); ++i) {
        const auto& l = lhs.at(i);
        const auto& r = rhs.at(i);
        if (std::string(l.Name) != std::string(r.Name))
            return {false,
                    describe(path,
                             "matrix " + std::to_string(i) + " name differs: original '" +
                                 std::string(l.Name) + "', exported '" + std::string(r.Name) +
                                 "'")};

        if (!same_bound(l.Data.t0, r.Data.t0) || !same_bound(l.Data.t1, r.Data.t1))
            return {false, describe(path, "matrix '" + std::string(l.Name) + "' bounds differ")};

        const auto original_grid = parse_credit_simulation_grid(l.Data);
        const auto exported_grid = parse_credit_simulation_grid(r.Data);

        // The exported document is expected to carry no state comment. The
        // rating names are held by the type, not by the document, so the mapper
        // does not write ORE's comment back and the round trip differs from the
        // original by that comment alone. Asserting the absence here records
        // the decision instead of hiding it behind a comparison that ignores it.
        if (exported_grid.labels.size() != 0)
            return {false,
                    describe(path,
                             "matrix '" + std::string(l.Name) + "' exported a state comment of " +
                                 std::to_string(exported_grid.labels.size()) +
                                 " labels, expected none")};

        if (original_grid.values.size() != exported_grid.values.size())
            return {false,
                    describe(path,
                             "matrix '" + std::string(l.Name) + "' cell count differs: original " +
                                 std::to_string(original_grid.values.size()) + ", exported " +
                                 std::to_string(exported_grid.values.size()))};

        const auto side = original_grid.side();
        for (std::size_t index = 0; index < original_grid.values.size(); ++index) {
            if (original_grid.values[index] != exported_grid.values[index]) {
                const auto row = side == 0 ? 0 : index / side;
                const auto col = side == 0 ? 0 : index % side;
                return {false,
                        describe(path,
                                 "matrix '" + std::string(l.Name) + "' cell (row " +
                                     std::to_string(row) + ", col " + std::to_string(col) +
                                     ") differs: original " +
                                     std::to_string(original_grid.values[index]) + ", exported " +
                                     std::to_string(exported_grid.values[index]))};
            }
        }
    }
    return {};
}

mismatch compare_entities(const creditsimulation& original,
                          const creditsimulation& exported,
                          const std::string& path) {
    const auto& lhs = original.Entities.Entity;
    const auto& rhs = exported.Entities.Entity;
    if (lhs.size() != rhs.size())
        return {false,
                describe(path,
                         "entity count differs: original " + std::to_string(lhs.size()) +
                             ", exported " + std::to_string(rhs.size()))};

    for (std::size_t i = 0; i < lhs.size(); ++i) {
        const auto& l = lhs.at(i);
        const auto& r = rhs.at(i);
        if (std::string(l.Name) != std::string(r.Name))
            return {false,
                    describe(path,
                             "entity " + std::to_string(i) + " name differs: original '" +
                                 std::string(l.Name) + "', exported '" + std::string(r.Name) +
                                 "'")};
        if (std::string(l.FactorLoadings) != std::string(r.FactorLoadings))
            return {false, describe(path, "entity '" + std::string(l.Name) + "' loadings differ")};
        if (std::string(l.TransitionMatrix) != std::string(r.TransitionMatrix))
            return {false, describe(path, "entity '" + std::string(l.Name) + "' matrix differs")};
        if (l.InitialState != r.InitialState)
            return {false,
                    describe(path, "entity '" + std::string(l.Name) + "' initial state differs")};
    }
    return {};
}

mismatch compare(const creditsimulation& original,
                 const creditsimulation& exported,
                 const std::string& path) {
    auto result = compare_matrices(original, exported, path);
    if (!result.equal)
        return result;

    result = compare_entities(original, exported, path);
    if (!result.equal)
        return result;

    // The netting sets are a comma separated list whose surrounding whitespace
    // is representation, so the codes are compared rather than the text. ORE
    // indents the list across lines and the export writes it on one.
    const auto codes_of = [](const std::string& text) {
        std::vector<std::string> codes;
        std::istringstream stream(text);
        std::string token;
        while (std::getline(stream, token, ',')) {
            const auto first = token.find_first_not_of(" \t\r\n");
            if (first == std::string::npos)
                continue;
            codes.push_back(token.substr(first, token.find_last_not_of(" \t\r\n") - first + 1));
        }
        return codes;
    };
    if (codes_of(std::string(original.NettingSetIds)) !=
        codes_of(std::string(exported.NettingSetIds)))
        return {false, describe(path, "netting set ids differ")};

    const auto& l = original.Risk;
    const auto& r = exported.Risk;
    if (l.Market != r.Market || l.Credit != r.Credit || l.ZeroMarketPnl != r.ZeroMarketPnl ||
        l.DoubleDefault != r.DoubleDefault)
        return {false, describe(path, "risk flags differ")};
    if (std::string(l.Evaluation) != std::string(r.Evaluation))
        return {false, describe(path, "risk evaluation differs")};
    if (std::string(l.CreditMode) != std::string(r.CreditMode))
        return {false, describe(path, "risk credit mode differs")};
    if (std::string(l.LoanExposureMode) != std::string(r.LoanExposureMode))
        return {false, describe(path, "risk loan exposure mode differs")};
    if (l.Seed != r.Seed || l.Paths != r.Paths)
        return {false, describe(path, "risk seed or paths differ")};

    return {};
}

} // namespace

std::string credit_simulation_difference(const creditsimulation& original,
                                         const creditsimulation& exported,
                                         const std::string& path) {
    const auto result = compare(original, exported, path);
    return result.equal ? std::string() : result.message;
}

}
