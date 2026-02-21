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
#ifndef ORES_QT_METHODOLOGY_DETAIL_DIALOG_HPP
#define ORES_QT_METHODOLOGY_DETAIL_DIALOG_HPP

#include <boost/uuid/uuid.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/methodology.hpp"

namespace Ui {
class MethodologyDetailDialog;
}

namespace ores::qt {

class MethodologyDetailDialog final : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.methodology_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit MethodologyDetailDialog(QWidget* parent = nullptr);
    ~MethodologyDetailDialog() override;

    void setClientManager(ClientManager* cm) { clientManager_ = cm; }
    void setUsername(const std::string& username) { username_ = username; }
    void setCreateMode(bool create);
    void setMethodology(const dq::domain::methodology& methodology);
    void setReadOnly(bool readOnly);

signals:
    void methodologySaved(const boost::uuids::uuid& id);
    void methodologyDeleted(const boost::uuids::uuid& id);

protected:
    QTabWidget* tabWidget() const override;
    QWidget* provenanceTab() const override;
    ProvenanceWidget* provenanceWidget() const override;

private slots:
    void onSaveClicked();
    void onDeleteClicked();

private:
    void setupConnections();
    void updateUiState();

    Ui::MethodologyDetailDialog* ui_;
    ClientManager* clientManager_;
    std::string username_;
    dq::domain::methodology methodology_;
    bool isCreateMode_;
    bool isReadOnly_;
};

}

#endif
