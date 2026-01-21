/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/RecencyPulseManager.hpp"

namespace ores::qt {

RecencyPulseManager::RecencyPulseManager(QObject* parent,
                                          int pulse_interval_ms,
                                          int max_pulse_cycles)
    : QObject(parent),
      timer_(new QTimer(this)),
      pulse_interval_ms_(pulse_interval_ms),
      max_pulse_cycles_(max_pulse_cycles) {
    timer_->setInterval(pulse_interval_ms_);
    connect(timer_, &QTimer::timeout, this, &RecencyPulseManager::on_timer_timeout);
}

void RecencyPulseManager::start_pulsing() {
    pulse_count_ = 0;
    pulse_state_ = false;
    if (!timer_->isActive()) {
        timer_->start();
    }
}

bool RecencyPulseManager::is_pulsing() const {
    return timer_->isActive();
}

void RecencyPulseManager::stop_pulsing() {
    timer_->stop();
    pulse_state_ = false;
    pulse_count_ = 0;
}

void RecencyPulseManager::on_timer_timeout() {
    pulse_state_ = !pulse_state_;
    pulse_count_++;

    emit pulse_state_changed(pulse_state_);

    // A complete cycle is one on + one off, so we need max_pulse_cycles_ * 2
    // state changes to complete the requested number of cycles.
    if (pulse_count_ >= max_pulse_cycles_ * 2) {
        timer_->stop();
        emit pulsing_complete();
    }
}

}
