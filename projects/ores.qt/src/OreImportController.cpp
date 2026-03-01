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
#include "ores.qt/OreImportController.hpp"
#include "ores.qt/OreImportWizard.hpp"

namespace ores::qt {

using namespace ores::logging;

OreImportController::OreImportController(ClientManager* clientManager,
                                          QObject* parent)
    : QObject(parent),
      clientManager_(clientManager) {}

void OreImportController::trigger(QWidget* parent) {
    BOOST_LOG_SEV(lg(), info) << "Opening ORE import wizard";

    OreImportWizard wizard(clientManager_, parent);

    if (wizard.exec() == QDialog::Accepted && wizard.importSuccess()) {
        BOOST_LOG_SEV(lg(), info) << "ORE import wizard accepted";

        const auto msg = QString("ORE import complete: %1 currencies, %2 portfolios, "
                                 "%3 books, %4 trades.")
            .arg(wizard.savedCurrencies())
            .arg(wizard.savedPortfolios())
            .arg(wizard.savedBooks())
            .arg(wizard.savedTrades());

        emit statusMessage(msg);
        emit importCompleted();
    } else {
        BOOST_LOG_SEV(lg(), info) << "ORE import wizard cancelled or failed";
    }
}

}
