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
#ifndef ORES_QT_DATASET_DETAIL_DIALOG_HPP
#define ORES_QT_DATASET_DETAIL_DIALOG_HPP

#include <boost/uuid/uuid.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/dataset.hpp"

namespace Ui {
class DatasetDetailDialog;
}

namespace ores::qt {

class DatasetDetailDialog final : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.dataset_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit DatasetDetailDialog(QWidget* parent = nullptr);
    ~DatasetDetailDialog() override;

    void setClientManager(ClientManager* cm) { clientManager_ = cm; }
    void setUsername(const std::string& username) { username_ = username; }
    void setCreateMode(bool create);
    void setDataset(const dq::domain::dataset& dataset);
    void setReadOnly(bool readOnly);
    void loadLookupData();

signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& error);
    void datasetSaved(const boost::uuids::uuid& id);
    void datasetDeleted(const boost::uuids::uuid& id);

private slots:
    void onSaveClicked();
    void onDeleteClicked();

private:
    void setupConnections();
    void updateUiState();

    Ui::DatasetDetailDialog* ui_;
    ClientManager* clientManager_;
    std::string username_;
    dq::domain::dataset dataset_;
    bool isCreateMode_;
    bool isReadOnly_;

    // Store methodology UUID to name mapping
    std::vector<std::pair<boost::uuids::uuid, std::string>> methodologies_;
};

}

#endif
