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
#ifndef ORES_QT_CURRENCY_HISTORY_MDI_WINDOW_HPP
#define ORES_QT_CURRENCY_HISTORY_MDI_WINDOW_HPP

#include <QWidget>
#include <memory>
#include "ores.comms/net/client.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "ores.qt/CurrencyHistoryDialog.hpp"

namespace ores::qt {

/**
 * @brief MDI window for displaying currency version history.
 */
class CurrencyHistoryMdiWindow : public QWidget {
    Q_OBJECT

private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.qt.currency_history_mdi_window");
        return instance;
    }

public:
    explicit CurrencyHistoryMdiWindow(const QString& iso_code,
                                      std::shared_ptr<comms::client> client,
                                      QWidget* parent = nullptr);
    ~CurrencyHistoryMdiWindow() override;

    QSize sizeHint() const override; // Provide optimal size based on dialog content

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

private:
    CurrencyHistoryDialog* historyWidget_;
};

}

#endif
