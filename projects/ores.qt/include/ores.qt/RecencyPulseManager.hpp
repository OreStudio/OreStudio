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
#ifndef ORES_QT_RECENCY_PULSE_MANAGER_HPP
#define ORES_QT_RECENCY_PULSE_MANAGER_HPP

#include <QTimer>
#include <QObject>

namespace ores::qt {

/**
 * @brief Manages the pulsing animation for recently-changed items in models.
 *
 * This class provides a consistent pulsing mechanism for highlighting newly
 * added or modified items in table models. The pulse alternates between on
 * and off states for a configurable number of cycles.
 *
 * Usage:
 * 1. Create a RecencyPulseManager as a member of your model
 * 2. Connect to pulse_state_changed to emit dataChanged for ForegroundRole
 * 3. Connect to pulsing_complete to clear your recency tracking data
 * 4. Call start_pulsing() when new items are detected
 * 5. Use is_pulse_on() in your data() method to determine highlight state
 */
class RecencyPulseManager final : public QObject {
    Q_OBJECT

public:
    /**
     * @brief Constructs a RecencyPulseManager.
     *
     * @param parent Parent QObject
     * @param pulse_interval_ms Interval between pulse state changes (default 500ms)
     * @param max_pulse_cycles Number of complete on/off cycles (default 6)
     */
    explicit RecencyPulseManager(QObject* parent = nullptr,
                                  int pulse_interval_ms = 500,
                                  int max_pulse_cycles = 6);

    /**
     * @brief Start or restart the pulsing animation.
     *
     * Resets the pulse count and starts the timer. If already pulsing,
     * restarts from the beginning.
     */
    void start_pulsing();

    /**
     * @brief Check if the pulse is currently in the "on" (highlighted) state.
     *
     * Use this in your model's data() method for ForegroundRole to determine
     * if a recently-changed item should be highlighted.
     *
     * @return true if items should be highlighted, false otherwise
     */
    [[nodiscard]] bool is_pulse_on() const { return pulse_state_; }

    /**
     * @brief Check if the manager is currently pulsing.
     *
     * @return true if the pulse timer is active
     */
    [[nodiscard]] bool is_pulsing() const;

    /**
     * @brief Stop the pulsing animation immediately.
     */
    void stop_pulsing();

signals:
    /**
     * @brief Emitted whenever the pulse state changes.
     *
     * Connect this to your model's dataChanged signal to update the view.
     * The parameter indicates the new pulse state (true = highlighted).
     */
    void pulse_state_changed(bool is_on);

    /**
     * @brief Emitted when the pulsing animation completes all cycles.
     *
     * Connect this to a slot that clears your recency tracking data
     * (e.g., recent_codes_.clear()).
     */
    void pulsing_complete();

private slots:
    void on_timer_timeout();

private:
    QTimer* timer_;
    bool pulse_state_{false};
    int pulse_count_{0};
    int pulse_interval_ms_;
    int max_pulse_cycles_;
};

}

#endif
