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
#ifndef ORES_QT_WORKUNIT_DETAIL_DIALOG_HPP
#define ORES_QT_WORKUNIT_DETAIL_DIALOG_HPP

#include <QUrl>
#include <QString>
#include <vector>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.compute.api/domain/batch.hpp"
#include "ores.compute.api/domain/app_version.hpp"
#include "ores.compute.api/domain/workunit.hpp"

namespace Ui {
class WorkunitDetailDialog;
}

namespace ores::qt {

/**
 * @brief Dialog for submitting a new workunit to the compute grid.
 *
 * Allows the user to specify the batch and app version UUIDs, upload
 * input and config files, and set scheduling parameters. On submit,
 * files are uploaded via HTTP and the workunit is registered via NATS.
 */
class WorkunitDetailDialog final : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.workunit_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit WorkunitDetailDialog(QWidget* parent = nullptr);
    ~WorkunitDetailDialog() override;

    void setClientManager(ClientManager* clientManager);
    void setUsername(const std::string& username);
    void setHttpBaseUrl(const std::string& url);
    void setWorkunit(const compute::domain::workunit& workunit);
    void setCreateMode(bool createMode);
    void setReadOnly(bool readOnly);

signals:
    void workunitSaved(const QString& id);
    void workunitDeleted(const QString& id);

private slots:
    void onSaveClicked();
    void onDeleteClicked();
    void onFieldChanged();
    void onBrowseInputClicked();
    void onBrowseConfigClicked();

protected:
    QTabWidget* tabWidget() const override;
    QWidget* provenanceTab() const override;
    ProvenanceWidget* provenanceWidget() const override;
    bool hasUnsavedChanges() const override { return hasChanges_; }

private:
    struct IdEntry {
        std::string id;
        std::string label;
    };

    void setupUi();
    void setupConnections();
    void loadBatches();
    void loadAppVersions();
    void populateBatchCombo();
    void populateAppVersionCombo();
    void updateUiFromWorkunit();
    void updateWorkunitFromUi();
    void updateSaveButtonState();
    bool validateInput();
    void saveWorkunitViaNats();

    Ui::WorkunitDetailDialog* ui_;
    ClientManager* clientManager_;
    std::string username_;
    QUrl httpBaseUrl_;
    compute::domain::workunit workunit_;
    std::vector<IdEntry> batchEntries_;
    std::vector<IdEntry> appVersionEntries_;
    QString selectedInputFilePath_;
    QString selectedConfigFilePath_;
    bool createMode_{true};
    bool readOnly_{false};
    bool hasChanges_{false};
};

}

#endif
