DIR = pipegame

CP = .:/home/gavin/Scala/lib/scala-swing_2.13-3.0.0.jar 

all: $(DIR)/PipeGame.class

clean: rm $(DIR)/*.class; fsc -shutdown

# $(DIR)/MazeTree.class: $(DIR)/Maze.class

$(DIR)/Model.class: $(DIR)/Piece.class $(DIR)/FrameT.class $(DIR)/LevelInfo.class $(DIR)/Concurrency.class

$(DIR)/BasePanel.class: $(DIR)/Piece.class

$(DIR)/PipePanel.class: $(DIR)/Model.class $(DIR)/BasePanel.class

$(DIR)/TopPanel.class: $(DIR)/BasePanel.class

$(DIR)/InfoPanel.class: $(DIR)/BasePanel.class $(DIR)/Model.class

$(DIR)/PipeFrame.class: $(DIR)/PipePanel.class $(DIR)/TopPanel.class $(DIR)/InfoPanel.class

$(DIR)/PipeGame.class: $(DIR)/PipeFrame.class

$(DIR)/%.class:	%.scala
	fsc -deprecation  -cp ${CP} $<
