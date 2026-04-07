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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_QT_PLUGIN_BASE_HPP
#define ORES_QT_PLUGIN_BASE_HPP

#include <QObject>
#include "ores.qt/export.hpp"
#include "ores.qt/IPlugin.hpp"

namespace ores::qt {

class DetachableMdiSubWindow;
class EntityController;

/**
 * @brief Concrete base class for all domain plugins.
 *
 * Inherits from both QObject and IPlugin so that domain plugin classes can
 * inherit a single base and gain:
 *
 *  - Common signals forwarded from entity controllers to the host MainWindow:
 *    statusMessage, windowCreated, windowDestroyed.
 *  - connectControllerSignals() — wires an EntityController's four signals to
 *    the above using modern functor syntax (no SIGNAL/SLOT macros).
 *
 * Domain plugins should inherit PluginBase, declare Q_OBJECT,
 * Q_PLUGIN_METADATA, and Q_INTERFACES, then call connectControllerSignals()
 * for every controller they create in on_login().
 */
class ORES_QT_API PluginBase : public QObject, public IPlugin {
    Q_OBJECT

public:
    explicit PluginBase(QObject* parent = nullptr);
    ~PluginBase() override = default;

signals:
    /** @brief Status or error message to display in the host status bar. */
    void statusMessage(const QString& msg);

    /** @brief A detachable MDI sub-window was created by a controller. */
    void windowCreated(DetachableMdiSubWindow* window);

    /** @brief A detachable MDI sub-window owned by a controller was destroyed. */
    void windowDestroyed(DetachableMdiSubWindow* window);

protected:
    /**
     * @brief Wire the four standard EntityController signals to PluginBase signals.
     *
     * Call this once for every controller created in on_login():
     * @code
     *   myController_ = std::make_unique<MyController>(...);
     *   connectControllerSignals(myController_.get());
     * @endcode
     */
    void connectControllerSignals(EntityController* ctrl);
};

}

#endif
