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
#ifndef ORES_QT_CATALOG_DETAIL_DIALOG_HPP
#define ORES_QT_CATALOG_DETAIL_DIALOG_HPP

#include <QWidget>
#include <string>
#include "ores.dq/domain/catalog.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"

namespace Ui {
class CatalogDetailDialog;
}

namespace ores::qt {

/**
 * @brief Dialog for viewing and editing catalog details.
 */
class CatalogDetailDialog : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.catalog_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit CatalogDetailDialog(QWidget* parent = nullptr);
    ~CatalogDetailDialog() override;

    void setClientManager(ClientManager* clientManager);
    void setUsername(const std::string& username);
    void setCatalog(const dq::domain::catalog& catalog);
    void setCreateMode(bool createMode);
    void setReadOnly(bool readOnly);

signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& message);
    void catalogSaved(const QString& name);
    void catalogDeleted(const QString& name);

private slots:
    void onSaveClicked();
    void onDeleteClicked();

private:
    void updateUiFromCatalog();
    void updateCatalogFromUi();

    Ui::CatalogDetailDialog* ui_;
    ClientManager* clientManager_;
    std::string username_;
    dq::domain::catalog catalog_;
    bool createMode_;
    bool readOnly_;
};

}

#endif
