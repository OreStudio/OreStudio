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
#include <QDateTime>
#include "ores.qt/EntityController.hpp"
#include "ores.refdata/domain/country.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

class DetachableMdiSubWindow;
class ImageCache;
class ChangeReasonCache;

/**
 * @brief Controller managing all country-related windows and operations.
 */
class CountryController : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.country_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit CountryController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        ClientManager* clientManager,
        ImageCache* imageCache,
        ChangeReasonCache* changeReasonCache,
        const QString& username,
        QObject* parent = nullptr);

    ~CountryController() override;

    void showListWindow() override;
    void closeAllWindows() override;
    void reloadListWindow() override;

private slots:
    void onAddNewRequested();
    void onShowCountryDetails(const refdata::domain::country& country);
    void onShowCountryHistory(const QString& alpha2Code);
    void onOpenCountryVersion(const refdata::domain::country& country, int versionNumber);
    void onRevertCountry(const refdata::domain::country& country);
    void onNotificationReceived(const QString& eventType, const QDateTime& timestamp,
                                const QStringList& entityIds);

private:
    ImageCache* imageCache_;
    ChangeReasonCache* changeReasonCache_;
    QPointer<DetachableMdiSubWindow> countryListWindow_;
};

}

#endif
