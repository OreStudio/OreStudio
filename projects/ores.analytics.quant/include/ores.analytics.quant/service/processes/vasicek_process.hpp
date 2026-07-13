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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_VASICEK_PROCESS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_VASICEK_PROCESS_HPP

#include "ores.analytics.quant/domain/i_yield_curve_process.hpp"
#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/service/processes/hull_white_process.hpp"
#include <cstdint>

namespace ores::analytics::quant::service {

/**
 * @brief Vasicek (1977) mean-reverting short-rate process.
 *
 * dr = kappa * (theta - r) * dt + sigma * dW
 *
 * The constant-theta special case of hull_white_process, which this class
 * composes rather than reimplements: Vasicek is Hull-White with theta(t)
 * held at one constant value for all time, not a separately-derived model.
 * See hull_white_process's docstring for the exact one-tick transition and
 * the discount_factor() recursion this delegates to.
 */
class ORES_ANALYTICS_QUANT_EXPORT vasicek_process final
    : public ores::analytics::quant::domain::IYieldCurveProcess {
public:
    vasicek_process(
        double kappa, double theta, double sigma, double initial_rate, std::uint32_t seed = 42);

    double next() override;
    double current() const override;
    double discount_factor(std::size_t ticks_ahead) const override;

private:
    hull_white_process inner_;
};

}

#endif
