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
#ifndef ORES_QT_ORE_IMPORT_CONTROLLER_HPP
#define ORES_QT_ORE_IMPORT_CONTROLLER_HPP

#include <QObject>
#include <QWidget>
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"

namespace ores::qt {

/**
 * @brief Controller that owns and launches the OreImportWizard.
 *
 * Created once per session (in MainWindow::createControllers) and wired to:
 *   - File → Import ORE Data… menu action
 *   - Portfolio Explorer toolbar import button
 *
 * Call trigger(parent) to display the wizard modally.
 */
class OreImportController final : public QObject {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.ore_import_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit OreImportController(ClientManager* clientManager,
                                  QObject* parent = nullptr);
    ~OreImportController() override = default;

    /**
     * @brief Show the ORE import wizard modal dialog.
     *
     * @param parent Widget to use as the dialog parent (for centering).
     */
    void trigger(QWidget* parent = nullptr);

signals:
    /**
     * @brief Emitted when an import completes successfully.
     *
     * Subscribers (e.g. PortfolioExplorerMdiWindow) can use this to reload data.
     */
    void importCompleted();

    /**
     * @brief Emitted with a human-readable summary after import.
     */
    void statusMessage(const QString& message);

private:
    ClientManager* clientManager_;
};

}

#endif
