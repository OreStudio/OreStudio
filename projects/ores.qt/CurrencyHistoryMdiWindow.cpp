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
#include <QVBoxLayout>
#include "ores.qt/CurrencyHistoryMdiWindow.hpp"

namespace ores::qt {

using namespace ores::utility::log;

CurrencyHistoryMdiWindow::CurrencyHistoryMdiWindow(const QString& iso_code,
                                                   std::shared_ptr<comms::client> client,
                                                   QWidget* parent)
    : QWidget(parent),
      historyWidget_(new CurrencyHistoryDialog(iso_code, std::move(client), this)) {

    BOOST_LOG_SEV(lg(), info) << "Creating currency history MDI window for: "
                              << iso_code.toStdString();

    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->addWidget(historyWidget_);

    // Forward signals
    connect(historyWidget_, &CurrencyHistoryDialog::statusChanged,
            this, &CurrencyHistoryMdiWindow::statusChanged);
    connect(historyWidget_, &CurrencyHistoryDialog::errorOccurred,
            this, &CurrencyHistoryMdiWindow::errorOccurred);

    // Load history data
    historyWidget_->loadHistory();
}

}
