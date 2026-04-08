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
#ifndef ORES_QT_PLUGIN_CONTEXT_HPP
#define ORES_QT_PLUGIN_CONTEXT_HPP

#include <memory>
#include <string>
#include <QString>
#include "ores.qt/export.hpp"

class QMainWindow;
class QMdiArea;
class QStatusBar;

namespace ores::eventing::service { class event_bus; }

namespace ores::qt {

class ClientManager;
class ImageCache;
class ChangeReasonCache;
class BadgeCache;

/**
 * @brief Context passed to each plugin at login time.
 *
 * Carries the shared application resources needed by domain plugins to create
 * their controllers and windows.  The host (MainWindow) owns all resources;
 * plugins must not delete any of these pointers.
 */
struct ORES_QT_API plugin_context {
    QMainWindow*  main_window         = nullptr;
    QMdiArea*     mdi_area            = nullptr;
    QStatusBar*   status_bar          = nullptr;
    ClientManager*      client_manager      = nullptr;
    ImageCache*         image_cache         = nullptr;
    ChangeReasonCache*  change_reason_cache = nullptr;
    BadgeCache*         badge_cache         = nullptr;
    std::shared_ptr<eventing::service::event_bus> event_bus;
    QString     username;
    std::string http_base_url;
};

}

#endif
