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
#ifndef ORES_QT_COUNTRY_CONTROLLER_HPP
#define ORES_QT_COUNTRY_CONTROLLER_HPP

#include <QPointer>
#include <QList>
#include <QDateTime>
#include "ores.qt/EntityController.hpp"
#include "ores.risk/domain/country.hpp"
#include "ores.telemetry/log/make_logger.hpp"

namespace ores::qt {

class DetachableMdiSubWindow;
class ImageCache;

/**
 * @brief Controller managing all country-related windows and operations.
 */
class CountryController : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.country_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit CountryController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        ClientManager* clientManager,
        ImageCache* imageCache,
        const QString& username,
        QList<DetachableMdiSubWindow*>& allDetachableWindows,
        QObject* parent = nullptr);

    ~CountryController() override;

    void showListWindow() override;
    void closeAllWindows() override;

private slots:
    void onAddNewRequested();
    void onShowCountryDetails(const risk::domain::country& country);
    void onShowCountryHistory(const QString& alpha2Code);
    void onNotificationReceived(const QString& eventType, const QDateTime& timestamp,
                                const QStringList& entityIds);

private:
    QList<DetachableMdiSubWindow*>& allDetachableWindows_;
    ImageCache* imageCache_;
    QPointer<DetachableMdiSubWindow> countryListWindow_;
};

}

#endif
