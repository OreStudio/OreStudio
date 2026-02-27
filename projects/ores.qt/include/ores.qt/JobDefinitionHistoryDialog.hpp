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
#ifndef ORES_QT_JOB_DEFINITION_HISTORY_DIALOG_HPP
#define ORES_QT_JOB_DEFINITION_HISTORY_DIALOG_HPP

#include <QWidget>
#include <QTableWidget>
#include <boost/uuid/uuid.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.scheduler/domain/job_instance.hpp"

namespace Ui {
class JobDefinitionHistoryDialog;
}

namespace ores::qt {

/**
 * @brief Dialog for viewing execution history (job_instance records) for a
 * job definition.
 *
 * Shows all pg_cron run records for the given job, fetched via
 * get_job_history_request.
 */
class JobDefinitionHistoryDialog final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.job_definition_history_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit JobDefinitionHistoryDialog(
        const boost::uuids::uuid& job_definition_id,
        const QString& job_name,
        ClientManager* clientManager,
        QWidget* parent = nullptr);
    ~JobDefinitionHistoryDialog() override;

    void loadHistory();

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

private slots:
    void onRunSelected();

private:
    void setupUi();
    void setupConnections();
    void updateRunList();

    Ui::JobDefinitionHistoryDialog* ui_;
    boost::uuids::uuid job_definition_id_;
    QString job_name_;
    ClientManager* clientManager_;
    std::vector<scheduler::domain::job_instance> instances_;
};

}

#endif
